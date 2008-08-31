// $Id$

package scala.continuations

import scala.tools.nsc._
import scala.tools.nsc.transform._
import scala.tools.nsc.symtab._
import scala.tools.nsc.plugins._

import scala.tools.nsc.ast._

/** 
 * CPS-transform marked assignments.
 */
abstract class TestTransform extends PluginComponent with Transform {
  // inherits abstract value `global' and class `Phase' from Transform

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import posAssigner.atPos         // for filling in tree positions 

  /** the following two members override abstract members in Transform */
  val phaseName: String = "earlytest"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new TestTransformer()



  lazy val MarkerCPS = definitions.getClass("scala.continuations.cps")
  lazy val Context = definitions.getClass("scala.continuations.Context")


  class TestTransformer extends Transformer {

    override def transform(tree: Tree): Tree = {
      tree match {
        
        case dd @ DefDef(mods, name, tparams, vparams, tpt, rhs) =>
          
          if (!mods.annotations.isEmpty)
            println("found annotated method: " + name + "/" + mods.annotations + "/" + dd)
          
          println("enter method " + name)
          
          val x = super.transform(tree)
          
          println("exit method " + name)
          
          x
          
        case vd @ ValDef(mods, name, tpt, rhs) =>

            if (!mods.annotations.isEmpty)
              println("annotated valdef at outer level...")

            super.transform(tree)

        case Apply(fun, args) =>

//          println("apply: " + getQualifiedType(fun).attributes)

          super.transform(tree)
        
        
        case Block(stms, expr) =>
        
          println("enter block")
          
          val (stms1, expr1) = transBlock(stms, expr)

          println("exit block")

          Block(stms1, expr1)
        
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
            case vd @ ValDef(mods, name, tpt, rhs) =>

              if (mods.annotations.contains(MarkerCPS)) {
                  println("found annotated valdef: " + name + "/" + mods.annotations + "/" + vd)
                                
//                val sym = currentMethod.newAnonymousFunctionClass(rhs.pos)
//                val arg = sym.newValueParameter(rhs.pos, name)


                val body = /*atOwner(sym)*/ {
                  val (a, b) = transBlock(rest, expr)
                  Block(a, b)
                }

//                new ChangeOwnerTraverser(currentMethod, sym).traverse(body)
//                new TreeSymSubstituter(List(vd.symbol), List(arg)).traverse(body)

                val fun = Function(
                  List(
                    ValDef(mods, name, tpt, EmptyTree)
                  ),
                  body
                ) //setSymbol(sym)

                val applied = Apply(
                  Select(
                    transform(rhs),
                    Context.tpe.member("extend")
                  ),
                  List(
                    fun
                  )
                )

                (Nil, applied)

              } else {
                val (a, b) = transBlock(rest, expr)
                (transform(stm)::a, b)
              }
              
            case _ => 
                val (a, b) = transBlock(rest, expr)
                (transform(stm)::a, b)
          }
       }
    }


  }
}
