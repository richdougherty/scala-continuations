// $Id$

package scala.continuations

import scala.tools.nsc.Global

abstract class CPSAnnotationChecker {
  val global: Global
  import global._
  import definitions._


  lazy val MarkerCPS = definitions.getClass("scala.continuations.cps")
  lazy val MarkerCPSTypes = definitions.getClass("scala.continuations.cpstypes")


  /** 
   *  Checks whether @cpstypes annotations conform
   */


  def filterAttribs(tpe:Type, cls:Symbol) = {
    def byClass(cls: Symbol)(a: AnnotationInfo) = a match {
      case AnnotationInfo(tp, _, _) if tp.typeSymbol == cls => true
      case _ => false
    }
      
    tpe.attributes.filter(byClass(cls))
  }


  object checker extends AnnotationChecker {

    /** Check annotations to decide whether tpe1 <:< tpe2 */
    def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {

      return true // FIXME: check what's the right thing to do


      val m1 = filterAttribs(tpe1,MarkerCPSTypes)
      val m2 = filterAttribs(tpe2,MarkerCPSTypes)
      
      (m1::: m2) match {
        case List(AnnotationInfo(t1,_,_), AnnotationInfo(t2,_,_)) =>
          println("checking two annotated types: "+t1+" <:< "+t2)
          t1 <:< t2
        case List(AnnotationInfo(_,_,_)) => 
          true
        case Nil => 
          true
        case _ =>
          false
      }
      
    }

    /** Modify the type that has thus far been inferred
     *  for a tree.  All this should do is add annotations. */
    override def addAnnotations(tree: Tree, tpe: Type): Type = {
//      println("adding annot to "+ tree.symbol + ": " + tpe)
//      if (tree.symbol != null) println("annots: " + tree.symbol.attributes + " / " + tpe.attributes)
      
      tpe
      /*
      
      tree match {
        case dd @ DefDef(mods, name, tparams, vparams, tpt, rhs) =>
          if (dd.symbol.hasAttribute(MarkerCPS)) {
	    println("adding annot to "+ tree.symbol + ": " + tpe)
            val plus1 = filterAttribs(tpe,MarkerCPS).length != 0
	    if (!plus1) {
              tpe.withAttribute(AnnotationInfo(MarkerCPS.tpe,Nil,Nil))
	    }
	    else
	      tpe
          }
              tpe
            case _ =>
              tpe
          }
*/
	
   /*
        tpe match {
            case compiler.AnnotatedType(attribs, _, _) =>
              attribs.find(isDimensionAnnotation) match {  // todo: only allow one dim annotation
                case None => WildcardDimension
                case Some(compiler.AnnotationInfo(_, List(dimexp), _)) => 
                  exp2dim(dimexp.tree)
                case Some(compiler.AnnotationInfo(_, _, _)) =>
                  Console.println("dim annotations should have exactly one argument and no refinement") // todo:proper error handling
                  WildcardDimension
              }
            case compiler.MethodType(Nil, result) => tpe2dim(result)
            case compiler.PolyType(Nil, result) => tpe2dim(result)
            case _ => WildcardDimension
          }


          println("->" + dd)
*/          
          
    }
  }
}
