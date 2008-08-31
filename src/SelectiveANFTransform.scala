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
  import typer.{atOwner}    // methods to type trees
  import posAssigner.atPos         // for filling in tree positions 

  /** the following two members override abstract members in Transform */
  val phaseName: String = "selectiveanf"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new ANFTransformer(unit)





  lazy val MarkerCPS = definitions.getClass("scala.continuations.cps")
  lazy val Context = definitions.getClass("scala.continuations.Context")


  class ANFTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    override def transform(tree: Tree): Tree = {
      tree match {
        
        case dd @ DefDef(mods, name, tparams, vparams, tpt, rhs)
        if (dd.symbol.hasAttribute(MarkerCPS)) =>

          log("transforming " + tree.symbol.fullNameString)

          val rhs1 = atOwner(dd.symbol) {
              toExpr(rhs, transTailExpr(rhs))
          }

          log("result "+rhs1)
          log("result is of type "+rhs1.tpe)

          copy.DefDef(dd, mods, name, tparams, vparams,
            tpt, rhs1)

        case _ => 
          super.transform(tree)
      }
    }



    def getQualifiedSymbol(tree: Tree): Symbol = {

      // need to find the annotations of a method symbol
      // TODO: is there a standard way of doing this?

      tree match {
        case Select(qual, name) =>
          
          val par = getQualifiedSymbol(qual)
          if (par != null && par.tpe != null) {
            par.tpe.member(name)
          } else
            tree.symbol
        case TypeApply(fun, args) =>
          getQualifiedSymbol(fun)
        case _ => tree.symbol

        // TODO: other cases
      }
    }

    def toExpr(orig: Tree, res: (List[Tree], Tree)) = {
      res match {
        case (Nil, b) => b
        case (a, b) =>
          copy.Block(orig, a,b)
      }
    }




    def transTailExpr(tree: Tree): (List[Tree], Tree) = {
      tree match {
        case Apply(fun, args) =>
          val (funStm, funExpr) = transExpr(fun)
          val (argStm, argExpr) = List.unzip(for (a <- args) yield transExpr(a))

          (funStm ::: List.flatten(argStm), copy.Apply(tree, funExpr, argExpr))
        case _ =>
          transExpr(tree)
      }
    }
      
    def transExpr(tree: Tree): (List[Tree], Tree) = {
      tree match {
        case Block(stms, expr) => 
          val (a, b) = transBlock(stms, expr)
          (Nil, copy.Block(tree, a, b))

        case ValDef(mods, name, tptTre, rhs) =>
          val (stms, anfRhs) = transExpr(rhs)
          (stms, copy.ValDef(tree, mods, name, tptTre, anfRhs))

        // FIXME: if valdef is already marked, don't translate it again
          
        case Select(qual, name) =>
          val (stms, expr) = transExpr(qual)
          (stms, copy.Select(tree, expr, name))

        case If(cond, thenp, elsep) =>
          val (condStats, condVal) = transExpr(cond)
          val thenVal = toExpr(thenp, transTailExpr(thenp))
          val elseVal = toExpr(elsep, transTailExpr(elsep))

          (condStats, copy.If(tree, condVal, thenVal, elseVal))

        case TypeApply(fun, args) =>
          val (stms, expr) = transExpr(fun)
          (stms, copy.TypeApply(tree, expr, args))

        case Apply(fun, args) =>
          val (funStm, funExpr) = transExpr(fun)
          val (argStm, argExpr) = List.unzip(for (a <- args) yield transExpr(a))
          
          // TODO: use transTailExpr for call-by-name parameters

          if (getQualifiedSymbol(fun).hasAttribute(MarkerCPS)) {

            log("found apply of "+fun+", which is marked")
            log("result of application has type " + tree.tpe)
            
            val sym = currentOwner.newValue(tree.pos, unit.fresh.newName(tree.pos, "tmp"))
                        .setInfo(tree.tpe)
                        .setFlag(Flags.SYNTHETIC)
                        .setAttributes(List(AnnotationInfo(MarkerCPS.tpe, Nil, Nil)))
          
            (funStm ::: List.flatten(argStm) ::: 
               List(ValDef(sym, copy.Apply(tree, funExpr, argExpr)) setType(NoType)),
               Ident(sym) setType(tree.tpe))

          } else {

            (funStm ::: List.flatten(argStm), copy.Apply(tree, funExpr, argExpr))
          
          }

        // TODO: other cases

        case _ =>
          (Nil, transform(tree))
      }
    }


    def transBlock(stms: List[Tree], expr: Tree): (List[Tree], Tree) = {

      stms match {
        case Nil =>
          transTailExpr(expr)

        case stm::rest =>
          val (headStm, headExpr) = transExpr(stm)
          val (restStm, restExpr) = transBlock(rest, expr)
          (headStm:::List(headExpr):::restStm, restExpr)
       }
    }


  }
}
