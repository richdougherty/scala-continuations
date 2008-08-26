// $Id$

package scala.continuations

import scala.tools.nsc._
import scala.tools.nsc.transform._
import scala.tools.nsc.symtab._
import scala.tools.nsc.plugins._

import scala.tools.nsc.ast.TreeBrowsers
import scala.tools.nsc.ast._

/** 
 * In methods marked @cps, explicitly name results of calls to other @cps methods
 */
abstract class SelectiveANFTransform extends PluginComponent with Transform with TypingTransformers {
  // inherits abstract value `global' and class `Phase' from Transform

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import typer.{typed, atOwner}    // methods to type trees
  import posAssigner.atPos         // for filling in tree positions 

  /** the following two members override abstract members in Transform */
  val phaseName: String = "selectiveanf"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new ANFTransformer(unit)





  lazy val Async = definitions.getClass("scala.continuations.cps")
  lazy val Shift = definitions.getClass("scala.continuations.Shift")


  class ANFTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    override def transform(tree: Tree): Tree = {
      tree match {
        
        case dd @ DefDef(mods, name, tparams, vparams, tpt, rhs)
        if (dd.symbol.hasAttribute(Async)) =>

          log("transforming " + tree.symbol.fullNameString)

          val rhs1 = atOwner(dd.symbol) {
              val (stms1, expr1) = rhs match {
              case Block(stms, expr) => transBlock(stms, expr)
              case expr => transBlock(Nil, expr)
            }
            typed(Block(stms1, expr1))
          }

          log("result "+rhs1)
          log("result is of type "+rhs1.tpe)

          copy.DefDef(dd, mods, name, tparams, vparams,
            TypeTree(rhs1.tpe), rhs1)

        case _ => 
          super.transform(tree)
      }
    }



    def getQualifiedType(tree: Tree): Symbol = {

      // need to find the annotations of a method symbol
      // probably this function already exists elsewhere

      tree match {
        case Select(qual, name) =>
          
          val par = getQualifiedType(qual)
          if (par != null && par.tpe != null) {
            par.tpe.member(name)
          } else
            tree.symbol
        case TypeApply(fun, args) =>
          getQualifiedType(fun)
        case _ => tree.symbol

        // TODO: other cases
      }
    }


    def transExpr(tree: Tree): (List[Tree], Tree) = {
      tree match {
        case Block(stms, expr) => 
          transBlock(stms, expr)

        case ValDef(mods, name, tptTre, rhs) =>
          val (stms, anfRhs) = transExpr(rhs)
          (stms, typed(ValDef(tree.symbol, anfRhs)))
          
        case Select(qual, name) =>
          val (stms, expr) = transExpr(qual)
          (stms, Select(expr, name))

        case TypeApply(fun, args) =>
          val (stms, expr) = transExpr(fun)
          (stms, TypeApply(fun, args))

        case Apply(fun, args) =>
          val (funStm, funExpr) = transExpr(fun)
          val (argStm, argExpr) = List.unzip(for (a <- args) yield transExpr(a))

          if (getQualifiedType(fun).hasAttribute(Async)) {

            log("found apply of "+fun+", which is marked")
            
            val sym = currentOwner.newValue(tree.pos, unit.fresh.newName(tree.pos, "tmp"))
                        .setInfo(tree.tpe)
                        .setFlag(Flags.SYNTHETIC)
                        .setAttributes(List(AnnotationInfo(Async.tpe, Nil, Nil)))
          
            (funStm ::: List.flatten(argStm) ::: 
               List(ValDef(sym, Apply(funExpr, argExpr))), Ident(sym))

          } else {

            (funStm ::: List.flatten(argStm), Apply(funExpr, argExpr))
          
          }

        // TODO: other cases

        case _ =>
          (Nil, transform(tree))
      }
    }


    def transBlock(stms: List[Tree], expr: Tree): (List[Tree], Tree) = {

      stms match {
        case Nil =>
          transExpr(expr)

        case stm::rest =>
          val (headStm, headExpr) = transExpr(stm)
          val (restStm, restExpr) = transBlock(rest, expr)
          (headStm:::List(headExpr):::restStm, restExpr)
       }
    }


  }
}
