package scala.continuations

import scala.tools.nsc.Global

abstract class CPSAnnotationChecker {
  val global: Global
  import global._

  /** An additional checker for annotations on types.
   *  Typically these are registered by compiler plugins
   *  with the addAnnotationChecker method. */

  object checker extends AnnotationChecker {
    /** Check the annotations on two types conform. */
    def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {
      println("checking: "+ tpe1 +" <: "+ tpe2)
      true
    }

    /** Modify the type that has thus far been inferred
     *  for a tree.  All this should do is add annotations. */
    override def addAnnotations(tree: Tree, tpe: Type): Type = {
      println("adding annot to "+ tree.symbol + ": " + tpe)
      
      tree match {
        case dd @ DefDef(mods, name, tparams, vparams, tpt, rhs) =>
          println("->" + dd)
        case _ =>
      }
      
      
      tpe
    }
  }
}
