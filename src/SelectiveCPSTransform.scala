// $Id$

package scala.continuations

import scala.tools.nsc._
import scala.tools.nsc.transform._
import scala.tools.nsc.plugins._

import scala.tools.nsc.ast.TreeBrowsers
import scala.tools.nsc.ast._

/** 
 * In methods marked @cps, CPS-transform assignments introduced by ANF-transform phase.
 */
abstract class SelectiveCPSTransform extends PluginComponent with Transform with TypingTransformers {
  // inherits abstract value `global' and class `Phase' from Transform

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import typer.{typed, atOwner}    // methods to type trees
  import posAssigner.atPos         // for filling in tree positions 

  /** the following two members override abstract members in Transform */
  val phaseName: String = "selectivecps"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new CPSTransformer(unit)





  lazy val Async = definitions.getClass("scala.continuations.cps")
  lazy val AsyncNoConvert = definitions.getClass("scala.continuations.cpsnoconvert")
  lazy val Shift = definitions.getClass("scala.continuations.Shift")


  class CPSTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

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
          log("method symbol was of type "+dd.symbol.tpe)
          

          def updateReturnType(tpe: Type, rhs: Type): Type = tpe match {
            case PolyType(a,b) => PolyType(a, updateReturnType(b,rhs))
            case MethodType(a,b) => MethodType(a, rhs)

            // FIXME: more cases?
          }


          dd.symbol.updateInfo(updateReturnType(dd.symbol.tpe, rhs1.tpe))

          log("method symbol updated to type "+dd.symbol.tpe)

          copy.DefDef(dd, mods, name, tparams, vparams, 
            TypeTree(rhs1.tpe), rhs1)

        case _ => 
          super.transform(tree)
      }
    }



    def transBlock(stms: List[Tree], expr: Tree): (List[Tree], Tree) = {

      stms match {
        case Nil =>
          (Nil, transform(expr))

        case stm::rest =>

          stm match {
            case vd @ ValDef(mods, name, tpt, rhs)
            if (vd.symbol.hasAttribute(Async) && !vd.symbol.hasAttribute(AsyncNoConvert)) =>

              log("found marked ValDef "+name+" of type " + vd.symbol.tpe)
              
              val tpe = if (tpt.tpe.typeSymbol == Shift) {
                AnyClass.tpe // FIXME
              } else {
                rhs.tpe
              }


              val sym = currentMethod.newAnonymousFunctionClass(rhs.pos)
              val arg = sym.newValueParameter(rhs.pos, name).setInfo(tpe)

              val body = typed(atOwner(sym) {
                val (a, b) = transBlock(rest, expr)
                Block(a, b)
              })
              

              // FIXME: don't know last parameter (result type of shift's body)
              val stpe = appliedType(Shift.tpe, List(tpe, body.tpe, AnyClass.tpe))

              log("computed shift will have type " + stpe)

              new ChangeOwnerTraverser(currentMethod, sym).traverse(body)
              new TreeSymSubstituter(List(vd.symbol), List(arg)).traverse(body)
              
              val fun = typed(Function(
                List(
                  ValDef(arg)
                ),
                body
              )) setSymbol(sym)
              
              // sym.setInfo(fun.tpe) // TODO: not needed?
              

              val applied = Apply(
                Select(
                  // FIXME: get rid of the type cast
                  TypeApply(
                    Select(super.transform(rhs), Any_asInstanceOf), 
                    List(TypeTree(stpe))
                  ),
//                super.transform(rhs),
                  Shift.tpe.member("bind")),
                List(
                  fun
                )
              )
              
              (Nil, applied)

            case _ => 
                val (a, b) = transBlock(rest, expr)
                (transform(stm)::a, b)
            }
      }
    }


  }
}
