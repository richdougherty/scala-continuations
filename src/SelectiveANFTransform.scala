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





  lazy val MarkerCPS = definitions.getClass("scala.continuations.cps")
  lazy val MarkerCPSTypes = definitions.getClass("scala.continuations.cpstypes")
  lazy val MarkerUnCPS = definitions.getClass("scala.continuations.uncps")
  lazy val Shift = definitions.getClass("scala.continuations.Shift")

  lazy val ModCPS = definitions.getModule("scala.continuations.CPS")
  lazy val MethShiftUnit = definitions.getMember(ModCPS, "shiftUnit")


  class ANFTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    override def transform(tree: Tree): Tree = {
      tree match {
        
        case dd @ DefDef(mods, name, tparams, vparams, tpt, rhs) =>
//        if (dd.symbol.hasAttribute(MarkerCPS)) =>

          log("transforming " + tree.symbol)

          val rhs1 = atOwner(dd.symbol) {
              transExpr(rhs, hasAnn(tpt.tpe))
          }

          log("result "+rhs1)
          log("result is of type "+rhs1.tpe)

          copy.DefDef(dd, mods, name, tparams, vparams,
            tpt, rhs1)

        case _ => 
        
          if (hasAnn(tree.tpe).isDefined)
            log(tree)
        
          super.transform(tree)
      }
    }


    def hasAnn(tp: Type) = {
     
      var res = AnyClass.tpe
      var outer = AnyClass.tpe
      var found = false

      tp.attributes.foreach {
        case AnnotationInfo(an, args, param) if an.typeSymbol == MarkerCPSTypes => 
          res = an.typeArgs(0)
          outer = an.typeArgs(1)
          found = true
        case _ =>
      }

      if (found)
        Some((res, outer))
      else
        None
     
//      tpe.attributes.isEmpty // TODO: check specificly
    }


    def transExpr(tree: Tree, cps: Option[(Type,Type)]): Tree = {
      transTailValue(tree, cps) match {
        case (Nil, b) => b
        case (a, b) =>
          copy.Block(tree, a,b).setType(b.tpe) // TODO: set type?
      }
    }


    def transParamList(fun: Tree, args: List[Tree]): (List[List[Tree]], List[Tree], Option[(Type,Type)]) = {
      val MethodType(formals, _) = fun.tpe
      val overshoot = args.length - formals.length
      
      var spc: Option[(Type,Type)] = None
      
      val (stm,expr) = List.unzip(for ((a,tp) <- args.zip(formals ::: List.make(overshoot, NoType))) yield {
        tp match {
          case TypeRef(_, sym, List(elemtp)) if sym == ByNameParamClass =>
            (Nil, transExpr(a, hasAnn(elemtp)))
          case _ =>
            val (valStm, valExpr, valSpc) = transInlineValue(a)
            spc = valSpc orElse spc
            (valStm, valExpr)
        }
      })
      
      (stm,expr,spc)
    }


    def transValue(tree: Tree, cps: Option[(Type,Type)]): (List[Tree], Tree, Option[(Type,Type)]) = {
      tree match {
        case Block(stms, expr) => 
          val (a, b) = transBlock(stms, expr, cps)
          (Nil, copy.Block(tree, a, b).setType(b.tpe), None)

        case ValDef(mods, name, tptTre, rhs) =>
          val (stms, anfRhs, spc) = atOwner(tree.symbol) { transInlineValue(rhs) }
          val tv = new ChangeOwnerTraverser(tree.symbol, currentOwner)
          stms.foreach(tv.traverse(_))
          (stms, copy.ValDef(tree, mods, name, tptTre, anfRhs), spc)

        // FIXME: if valdef is already marked, don't translate it again
        
        // valdef is not an expression (can't trans to a value), but a statement
          
        case Select(qual, name) =>
          val (stms, expr, spc) = transInlineValue(qual)
          (stms, copy.Select(tree, expr, name), spc)

        case If(cond, thenp, elsep) =>
          val (condStats, condVal, spc) = transInlineValue(cond)
          val thenVal = transExpr(thenp, cps)
          val elseVal = transExpr(elsep, cps)

          (condStats, copy.If(tree, condVal, thenVal, elseVal), spc)

        case TypeApply(fun, args) =>
          val (stms, expr, spc) = transInlineValue(fun)
          (stms, copy.TypeApply(tree, expr, args), spc)

        case Apply(fun, args) =>
            val (funStm, funExpr, funSpc) = transInlineValue(fun)
            val (argStm, argExpr, argSpc) = transParamList(fun, args)

            (funStm ::: List.flatten(argStm), copy.Apply(tree, funExpr, argExpr), argSpc orElse funSpc)

        case _ =>
          (Nil, transform(tree), None)
      }
    }
    
    def transTailValue(tree: Tree, cps: Option[(Type,Type)]): (List[Tree], Tree) = {
      
      val (stms, expr, spc) = transValue(tree, cps)

      // TODO: more general handling of throw expressions

      // FIXME: make sure everything matches!

      if ((cps.isDefined || spc.isDefined) && !(hasAnn(expr.tpe).isDefined || expr.isInstanceOf[Throw])) {
          log("cps type error (expected: " + cps + "/"+ spc + ") in: " + expr)

          val Some((a, b)) = cps orElse spc

          val res = atPos(expr.pos) {
	          localTyper.typed(Apply(TypeApply(gen.mkAttributedRef(MethShiftUnit), 
                    List(TypeTree(expr.tpe), TypeTree(a), TypeTree(b))), List(expr)))
          }

          (stms, res) // TODO: apply conversion
      } else {
        (stms, expr)
      }
      
    }
    
    def transInlineValue(tree: Tree): (List[Tree], Tree, Option[(Type,Type)]) = {
      
      val (stms, expr, spc) = transValue(tree, None) // never required to be cps

      hasAnn(expr.tpe) match {
        case spcVal @ Some(_) =>

          val valueTpe = expr.tpe.withoutAttributes // TODO: ok to remove all?

          val sym = currentOwner.newValue(tree.pos, unit.fresh.newName(tree.pos, "tmp"))
                      .setInfo(valueTpe)
                      .setFlag(Flags.SYNTHETIC)
                      .setAttributes(List(AnnotationInfo(MarkerCPS.tpe, Nil, Nil)))        

          (stms ::: List(ValDef(sym, expr) setType(NoType)),
             Ident(sym) setType(valueTpe) setPos(tree.pos), spcVal)

        case _ =>
          (stms, expr, spc)
      }
/*      
      tree match {

        case Apply(fun, args)
        if (getQualifiedSymbol(fun).hasAttribute(MarkerUnCPS)) =>

          val (funStm, funExpr) = transInlineValue(fun)
          val (argStm, argExpr) = transParamList(fun, args)

          val valueTpe = tree.tpe

          val sym = currentOwner.newValue(tree.pos, unit.fresh.newName(tree.pos, "tmp"))
                      .setInfo(valueTpe)
                      .setFlag(Flags.SYNTHETIC)
                      .setAttributes(List(AnnotationInfo(MarkerCPS.tpe, Nil, Nil)))        

          (funStm ::: List.flatten(argStm) ::: 
             List(ValDef(sym, copy.Apply(tree, funExpr, argExpr)) setType(NoType)),
             Ident(sym) setType(valueTpe) setPos(tree.pos))          


        case Apply(fun, args)
        if (getQualifiedSymbol(fun).hasAttribute(MarkerCPS)) =>
        
          val (funStm, funExpr) = transInlineValue(fun)
          val (argStm, argExpr) = transParamList(fun, args)
          
          log("found apply of "+fun+", which is marked")
          log("result of application has type " + tree.tpe)
          
          val valueTpe = tree.tpe

          val sym = currentOwner.newValue(tree.pos, unit.fresh.newName(tree.pos, "tmp"))
                      .setInfo(valueTpe)
                      .setFlag(Flags.SYNTHETIC)
                      .setAttributes(List(AnnotationInfo(MarkerCPS.tpe, Nil, Nil)))        

          (funStm ::: List.flatten(argStm) ::: 
             List(ValDef(sym, copy.Apply(tree, funExpr, argExpr)) setType(NoType)),
             Ident(sym) setType(valueTpe) setPos(tree.pos))

        case _ =>
          transTailValue(tree)
      }
*/      
    }


    def transBlock(stms: List[Tree], expr: Tree, cps: Option[(Type,Type)]): (List[Tree], Tree) = {

      stms match {
        case Nil =>
          transTailValue(expr, cps)

        case stm::rest =>
          val (headStm, headExpr, headSpc) = transInlineValue(stm)
/*          
          val xx = for (val tree@ValDef(_, _, _, _) <- headStm:::List(headExpr) if (tree.symbol.hasAttribute(MarkerCPS))) yield(0)
          
          log("block prefix: " + (headStm:::List(headExpr)))
          if (!xx.isEmpty)
            log("will translate block rest: " + rest)
          
          val cpsrest = cps || !xx.isEmpty
*/          
          val (restStm, restExpr) = transBlock(rest, expr, cps orElse headSpc)
          (headStm:::List(headExpr):::restStm, restExpr)
       }
    }


  }
}
