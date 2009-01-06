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
abstract class SelectiveANFTransform extends PluginComponent with Transform with
  TypingTransformers with CPSUtils {
  // inherits abstract value `global' and class `Phase' from Transform

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import typer.atOwner             // methods to type trees
  import posAssigner.atPos         // for filling in tree positions 

  /** the following two members override abstract members in Transform */
  val phaseName: String = "selectiveanf"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new ANFTransformer(unit)


  class ANFTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    var cpsAllowed: Boolean = false // detect cps code in places we do not handle yet

    override def transform(tree: Tree): Tree = {
      tree match {

        // TODO: Maybe we should generalize the transform and move it over to
        // the regular Transformer facility. But then, actual and required cps
        // state would need more complicated (stateful!) tracking. 
        
        // Making the default case use transExpr(tree, None, None) instead of
        // calling super.transform() would be a start, but at the moment,
        // this would cause infinite recursion. But we could remove the
        // ValDef case here.
        
        case dd @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          log("transforming " + dd.symbol)

          atOwner(dd.symbol) {
            val rhs1 = transExpr(rhs, None, getAnswerTypeAnn(tpt.tpe))
      
            log("result "+rhs1)
            log("result is of type "+rhs1.tpe)

            copy.DefDef(dd, mods, name, transformTypeDefs(tparams), transformValDefss(vparamss),
                        transform(tpt), rhs1)
          }

        case ff @ Function(vparams, body) =>
          log("transforming anon function " + ff.symbol)

          atOwner(ff.symbol) {
            val body1 = transExpr(body, None, getAnswerTypeAnn(body.tpe))
    
            log("result "+body1)
            log("result is of type "+body1.tpe)

            copy.Function(ff, transformValDefs(vparams), body1)
          }

        case vd @ ValDef(mods, name, tpt, rhs) => // object-level valdefs
          log("transforming valdef " + vd.symbol)

          atOwner(vd.symbol) {

            assert(getAnswerTypeAnn(tpt.tpe) == None)

            val rhs1 = transExpr(rhs, None, None)

            copy.ValDef(vd, mods, name, transform(tpt), rhs1)
          }

        case TypeTree() =>
          // circumvent cpsAllowed here
          super.transform(tree)
        
        case _ => 
          
          if (hasAnswerTypeAnn(tree.tpe)) {
            if (!cpsAllowed)
              unit.error(tree.pos, "cps code not allowed here / " + tree.getClass + " / " + tree)

            log(tree)
          }

          cpsAllowed = false
          super.transform(tree)            
      }
    }


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
          val (cpsA2, cpsR2) = (None, getAnswerTypeAnn(tree.tpe))
        
          val (a, b) = transBlock(stms, expr, cpsA2, cpsR2)
          
          val tree1 = copy.Block(tree, a, b) // no updateSynthFlag here!!!

          (Nil, tree1, cpsA) //cpsR)

        case If(cond, thenp, elsep) =>
          
          val (condStats, condVal, spc) = transInlineValue(cond, cpsA)

          val (cpsA2, cpsR2) = (None, getAnswerTypeAnn(tree.tpe))
          val thenVal = transExpr(thenp, cpsA2, cpsR2)
          val elseVal = transExpr(elsep, cpsA2, cpsR2)
          
          // TODO: check that then and else parts agree
          
          if (cpsR.isDefined) {
            if (elsep == EmptyTree)
              unit.error(tree.pos, "always need else part in cps code")
          }
  
          if (hasAnswerTypeAnn(thenVal.tpe) != hasAnswerTypeAnn(elseVal.tpe)) {
            unit.error(tree.pos, "then and else parts must both be cps code or neither of them")
          }

          (condStats, updateSynthFlag(copy.If(tree, condVal, thenVal, elseVal)), spc)

        case Match(selector, cases) =>
        
          val (selStats, selVal, spc) = transInlineValue(selector, cpsA)
          val (cpsA2, cpsR2) = (None, getAnswerTypeAnn(tree.tpe))

          val caseVals = for {
            cd @ CaseDef(pat, guard, body) <- cases
            val bodyVal = transExpr(body, cpsA2, cpsR2)
          } yield {
            copy.CaseDef(cd, transform(pat), transform(guard), bodyVal)
          }
          
          (selStats, updateSynthFlag(copy.Match(tree, selVal, caseVals)), spc)


        case LabelDef(name, params, rhs) =>
           // no cps code allowed in while loop!
          val rhsVal = transExpr(rhs, None, None)

          (Nil, updateSynthFlag(copy.LabelDef(tree, name, params, rhsVal)), cpsA)
          

        case Try(block, catches, finalizer) =>
          // no cps code allowed in try/catch/finally!
          val blockVal = transExpr(block, None, None)
          
          val catchVals = for {
            cd @ CaseDef(pat, guard, body) <- catches
            val bodyVal = transExpr(body, None, None)
          } yield {
            copy.CaseDef(cd, transform(pat), transform(guard), bodyVal)
          }

          val finallyVal = transExpr(finalizer, None, None)
          
          (Nil, updateSynthFlag(copy.Try(tree, blockVal, catchVals, finallyVal)), cpsA)

        case Assign(lhs, rhs) =>
          // allow cps code in rhs only
          val (stms, expr, spc) = transInlineValue(rhs, cpsA)
          (stms, updateSynthFlag(copy.Assign(tree, transform(lhs), expr)), spc)
          
        case Return(expr0) =>
          val (stms, expr, spc) = transInlineValue(expr0, cpsA)
          (stms, updateSynthFlag(copy.Return(tree, expr)), spc)

        case Throw(expr0) =>
          val (stms, expr, spc) = transInlineValue(expr0, cpsA)
          (stms, updateSynthFlag(copy.Throw(tree, expr)), spc)

        case Typed(expr0, tpt) =>
          // TODO: treat x: A @cps[B,C] specially?
          val (stms, expr, spc) = transInlineValue(expr0, cpsA)
          (stms, updateSynthFlag(copy.Typed(tree, expr, tpt)), spc)

        case TypeApply(fun, args) =>
          val (stms, expr, spc) = transInlineValue(fun, cpsA)
          (stms, updateSynthFlag(copy.TypeApply(tree, expr, args)), spc)

        case Select(qual, name) =>
          val (stms, expr, spc) = transInlineValue(qual, cpsA)
          (stms, updateSynthFlag(copy.Select(tree, expr, name)), spc)

        case Apply(fun, args) =>
          val (funStm, funExpr, funSpc) = transInlineValue(fun, cpsA)
          val (argStm, argExpr, argSpc) = transArgList(fun, args, funSpc)

          (funStm ::: List.flatten(argStm), updateSynthFlag(copy.Apply(tree, funExpr, argExpr)),
            argSpc)

        case _ =>
          cpsAllowed = true
          (Nil, transform(tree), cpsA)
      }
    }
    
    def transTailValue(tree: Tree, cpsA: CPSInfo, cpsR: CPSInfo): (List[Tree], Tree) = {
      
      val (stms, expr, spc) = transValue(tree, cpsA, cpsR)

      val bot = linearize(spc, getAnswerTypeAnn(expr.tpe))

      if (cpsR.isDefined && !bot.isDefined) {
        
        if (!expr.isEmpty && (expr.tpe.typeSymbol ne NothingClass)) {
          // must convert!
          log("cps type conversion (has: " + cpsA + "/" + spc + "/" + expr.tpe  + ")")
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
        
        // TODO: check that types agree
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
             Ident(sym) setType(valueTpe) setPos(tree.pos), linearize(spc, spcVal))

        case _ =>
          (stms, expr, spc)
      }

    }



    def transInlineStm(stm: Tree, cpsA: CPSInfo):  (List[Tree], CPSInfo) = {
      stm match {

        // TODO: what about DefDefs?
        // TODO: relation to top-level val def?

        case tree @ ValDef(mods, name, tpt, rhs) =>
          val (stms, anfRhs, spc) = atOwner(tree.symbol) { transValue(rhs, cpsA, None) }
        
          val tv = new ChangeOwnerTraverser(tree.symbol, currentOwner)
          stms.foreach(tv.traverse(_))

          // TODO: symbol might already have annotation. Should check conformance
          // TODO: better yet: do without annotations on symbols
          
          val spcVal = getAnswerTypeAnn(anfRhs.tpe)
          if (spcVal.isDefined) {
              tree.symbol.setAttributes(List(AnnotationInfo(MarkerCPS.tpe, Nil, Nil))) // TODO: don't remove others!
          }
          
          (stms:::List(copy.ValDef(tree, mods, name, tpt, anfRhs)), linearize(spc, spcVal))

        case _ =>
          val (headStms, headExpr, headSpc) = transInlineValue(stm, cpsA)
          val valSpc = getAnswerTypeAnn(headExpr.tpe)
          (headStms:::List(headExpr), linearize(headSpc, valSpc))
      }
    }

    def transBlock(stms: List[Tree], expr: Tree, cpsA: CPSInfo, cpsR: CPSInfo): (List[Tree], Tree) = {
      stms match {
        case Nil =>
          transTailValue(expr, cpsA, cpsR)

        case stm::rest =>
          var (rest2, expr2) = (rest, expr)
          val (headStms, headSpc) = transInlineStm(stm, cpsA)
          val (restStms, restExpr) = transBlock(rest2, expr2, headSpc, cpsR)
          (headStms:::restStms, restExpr)
       }
    }


  }
}
