// $Id$

package scala.continuations

import scala.tools.nsc._
import scala.tools.nsc.transform._
import scala.tools.nsc.symtab._
import scala.tools.nsc.plugins._

import scala.tools.nsc.ast._

/** 
 * In methods marked @cps, explicitly name results of calls to other @cps methods
 */
abstract class SelectiveANFTransform extends PluginComponent with Transform with TypingTransformers {
  // inherits abstract value `global' and class `Phase' from Transform

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import typer.{atOwner}           // methods to type trees
  import posAssigner.atPos         // for filling in tree positions 

  /** the following two members override abstract members in Transform */
  val phaseName: String = "selectiveanf"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new ANFTransformer(unit)



  lazy val MarkerCPS = definitions.getClass("scala.continuations.cpsv")
  lazy val MarkerCPSTypes = definitions.getClass("scala.continuations.cpstypes")
  lazy val MarkerCPSSynth = definitions.getClass("scala.continuations.uncps")
  lazy val Shift = definitions.getClass("scala.continuations.Shift")

  lazy val ModCPS = definitions.getModule("scala.continuations.CPS")
  lazy val MethShiftUnit = definitions.getMember(ModCPS, "shiftUnit")


  class ANFTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    override def transform(tree: Tree): Tree = {
      tree match {
        
        case dd @ DefDef(mods, name, tparams, vparamss, tpt, rhs)
          if (!dd.symbol.isConstructor) => // TODO: remove? This is just to make output less verbose...

          log("transforming " + dd.symbol)

          atOwner(dd.symbol) {
            val rhs1 = transExpr(rhs, None, getAnswerTypeAnn(tpt.tpe))
      
            log("result "+rhs1)
            log("result is of type "+rhs1.tpe)

            copy.DefDef(dd, mods, name, transformTypeDefs(tparams), transformValDefss(vparamss),
                        transform(tpt), rhs1)
          }

        case _ => 
        
          if (hasAnswerTypeAnn(tree.tpe))
            log(tree)
        
          super.transform(tree)
      }
    }


    def getAnswerTypeAnn(tp: Type) = {
      // TODO: what about multiple annotations?
      tp.attributes.find(a => a.atp.typeSymbol == MarkerCPSTypes) match {
        case Some(AnnotationInfo(atp, _, _)) => Some((atp.typeArgs(0), atp.typeArgs(1)))
        case None => None
      }
    }

    def hasAnswerTypeAnn(tp: Type) = {
      tp.attributes.exists(a => a.atp.typeSymbol == MarkerCPSTypes)
    }

    def hasSynthAnn(tp: Type) = {
      tp.attributes.exists(a => a.atp.typeSymbol == MarkerCPSSynth)
    }

    def updateSynthFlag(tree: Tree) = {
      if (hasSynthAnn(tree.tpe)) {
        log("removing annotation from " + tree)
        tree.setType(tree.tpe.withoutAttributes) // TODO: remove only ours
      } else
        tree
    }



    type CPSInfo = Option[(Type,Type)]

    def transExpr(tree: Tree, cpsA: CPSInfo, cpsR: CPSInfo): Tree = {
      transTailValue(tree, cpsA, cpsR) match {
        case (Nil, b) => b
        case (a, b) =>
          copy.Block(tree, a,b)
      }
    }


    def transArgList(fun: Tree, args: List[Tree], cpsA: CPSInfo): (List[List[Tree]], List[Tree], CPSInfo) = {
      val formals = fun.tpe.paramTypes
      val overshoot = args.length - formals.length
      
      var spc: CPSInfo = cpsA
      
      val (stm,expr) = List.unzip(for ((a,tp) <- args.zip(formals ::: List.make(overshoot, NoType))) yield {
        tp match {
          case TypeRef(_, sym, List(elemtp)) if sym == ByNameParamClass =>
            (Nil, transExpr(a, None, getAnswerTypeAnn(elemtp)))
          case _ =>
            val (valStm, valExpr, valSpc) = transInlineValue(a, spc)
            spc = valSpc
            (valStm, valExpr)
        }
      })
      
      (stm,expr,spc)
    }


    def transValue(tree: Tree, cpsA: CPSInfo, cpsR: CPSInfo): (List[Tree], Tree, CPSInfo) = {
      tree match {
        case Block(stms, expr) => 
          val (a, b) = transBlock(stms, expr, cpsA, cpsR)
          (Nil, copy.Block(tree, a, b), cpsR)

        case If(cond, thenp, elsep) =>
          val (condStats, condVal, spc) = transInlineValue(cond, cpsA)
          val thenVal = transExpr(thenp, spc, cpsR)
          val elseVal = transExpr(elsep, spc, cpsR)
//          val elseVal = transExpr(if (cps.isDefined && elsep == EmptyTree)
//          Literal(()).setType(UnitClass.tpe).setPos(tree.pos) else elsep, cps)
        
          // TODO: check that then and else parts agree
          
          if (cpsR.isDefined) {
            if (elsep == EmptyTree)
              unit.error(tree.pos, "always need else part in cps code")
          }
          
          if (hasAnswerTypeAnn(thenVal.tpe) != hasAnswerTypeAnn(elseVal.tpe)) {
              unit.error(tree.pos, "then and else part must both be cps code or neither of them")
          }

          (condStats, updateSynthFlag(copy.If(tree, condVal, thenVal, elseVal)), cpsR)

        case LabelDef(name, params, rhs) =>
          val rhsVal = transExpr(rhs, cpsA, cpsR)

          // FIXME: ref to declared label still carries old type
        
          if (hasAnswerTypeAnn(rhsVal.tpe)) {
            unit.error(tree.pos, "cps code not (yet) allowed in while loops")
          }
          
          (Nil, copy.LabelDef(tree, name, params, rhsVal), cpsR)
          
        
        case Select(qual, name) =>
          val (stms, expr, spc) = transInlineValue(qual, cpsA)
          (stms, updateSynthFlag(copy.Select(tree, expr, name)), spc)

        case TypeApply(fun, args) =>
          val (stms, expr, spc) = transInlineValue(fun, cpsA)
          (stms, updateSynthFlag(copy.TypeApply(tree, expr, args)), spc)

        case Apply(fun, args) =>
          val (funStm, funExpr, funSpc) = transInlineValue(fun, cpsA)
          val (argStm, argExpr, argSpc) = transArgList(fun, args, funSpc)

          (funStm ::: List.flatten(argStm), updateSynthFlag(copy.Apply(tree, funExpr, argExpr)),
            argSpc)

        case _ =>
          (Nil, transform(tree), None)
      }
    }
    
    def transTailValue(tree: Tree, cpsA: CPSInfo, cpsR: CPSInfo): (List[Tree], Tree) = {
      
      val (stms, expr, spc) = transValue(tree, cpsA, cpsR)

      val bot = getAnswerTypeAnn(expr.tpe) orElse spc

      if (cpsR.isDefined && !bot.isDefined) {
        
        if (expr.tpe.typeSymbol ne NothingClass) {
          // must convert!
          log("cps type conversion (expected: " + cpsR.get + "): " + expr)
          
          try {
            val Some((a, b)) = cpsR

            val res = localTyper.typed(atPos(tree.pos) {
                    Apply(TypeApply(gen.mkAttributedRef(MethShiftUnit), 
                      List(TypeTree(expr.tpe), TypeTree(a), TypeTree(b))), 
                       List(expr))
            })
            return (stms, res)

          } catch {
            case ex:TypeError =>
              unit.error(ex.pos, "cannot cps-transform expression " + tree + ": " + ex.msg)
          }
        }

      } else if (!cpsR.isDefined && bot.isDefined) {
        // error!
        log("cps type error: " + expr)
        unit.error(tree.pos, "found cps expression in non-cps position")

      } else {
        // all is well
        
        // TODO: check that types agree. but need to get rid of 'orElse's before
      }

      (stms, expr)
    }
    
    def transInlineValue(tree: Tree, cpsA: CPSInfo): (List[Tree], Tree, CPSInfo) = {

      val (stms, expr, spc) = transValue(tree, cpsA, None) // never required to be cps

      getAnswerTypeAnn(expr.tpe) match {
        case spcVal @ Some(_) =>

          val valueTpe = expr.tpe.withoutAttributes // TODO: remove only ours!

          val sym = currentOwner.newValue(tree.pos, unit.fresh.newName(tree.pos, "tmp"))
                      .setInfo(valueTpe)
                      .setFlag(Flags.SYNTHETIC)
                      .setAttributes(List(AnnotationInfo(MarkerCPS.tpe, Nil, Nil)))        

          (stms ::: List(ValDef(sym, expr) setType(NoType)),
             Ident(sym) setType(valueTpe) setPos(tree.pos), spcVal orElse spc)

        case _ =>
          (stms, expr, spc)
      }

    }


    def transBlock(stms: List[Tree], expr: Tree, cpsA: CPSInfo, cpsR: CPSInfo): (List[Tree], Tree) = {
      stms match {
        case Nil =>
          transTailValue(expr, cpsA, cpsR)

        case stm::rest =>
          var (rest2, expr2) = (rest, expr)
          val (headStms, headSpc) = stm match {

            // TODO: what about DefDefs?

            case tree@ValDef(mods, name, tpt, rhs) =>
              val (stms, anfRhs, spc) = atOwner(tree.symbol) { transValue(rhs, cpsA, None) }
            
              val tv = new ChangeOwnerTraverser(tree.symbol, currentOwner)
              stms.foreach(tv.traverse(_))

              // TODO: symbol might already have annotation. Should check conformance

              if (getAnswerTypeAnn(anfRhs.tpe).isDefined) {
                tree.symbol.setAttributes(List(AnnotationInfo(MarkerCPS.tpe, Nil, Nil))) // TODO: don't remove others!
              }
              
              (stms:::List(copy.ValDef(tree, mods, name, tpt, anfRhs)), spc)

            case _ =>
              val (headStms, headExpr, headSpc) = transInlineValue(stm, cpsA)
              (headStms:::List(headExpr), headSpc)
          }
          val (restStms, restExpr) = transBlock(rest2, expr2, headSpc, cpsR)
          (headStms:::restStms, restExpr)
       }
    }


  }
}
