// $Id$

package scala.continuations

import scala.tools.nsc.Global

trait CPSUtils {
  val global: Global
  import global._
  import definitions._

  val verbose: Boolean = false
  def vprintln(x: =>Any): Unit = if (verbose) println(x)


  lazy val MarkerCPS = definitions.getClass("scala.continuations.cpsv")
  lazy val MarkerCPSTypes = definitions.getClass("scala.continuations.cps")
  lazy val MarkerCPSSynth = definitions.getClass("scala.continuations.uncps")

  lazy val Shift = definitions.getClass("scala.continuations.ControlContext")
  lazy val Context = definitions.getClass("scala.continuations.ControlContext")

  lazy val ModCPS = definitions.getModule("scala.continuations.ControlContext")
  lazy val MethShiftUnit = definitions.getMember(ModCPS, "shiftUnit")
  lazy val MethShiftUnitR = definitions.getMember(ModCPS, "shiftUnitR")
  lazy val MethShift = definitions.getMember(ModCPS, "shift")
  lazy val MethShiftR = definitions.getMember(ModCPS, "shiftR")
  lazy val MethReify = definitions.getMember(ModCPS, "reify")
  lazy val MethReifyR = definitions.getMember(ModCPS, "reifyR")


  // annotation checker
  
  def filterAttribs(tpe:Type, cls:Symbol) = {
    def byClass(cls: Symbol)(a: AnnotationInfo) = a match {
      case AnnotationInfo(tp, _, _) if tp.typeSymbol == cls => true
      case _ => false
    }
      
    tpe.attributes.filter(byClass(cls)) // FIXME: prevent NPE!!
  }

  def linearize(ann: List[AnnotationInfo]): List[AnnotationInfo] = {
    // TODO: must check type conformance (--> flatMap/map)
    val synth = ann.filter(a => a.atp.typeSymbol == MarkerCPSSynth) match {
      case x::xs => List(x)
      case Nil => List()
    }
    val types = ann.filter(a => a.atp.typeSymbol == MarkerCPSTypes) match {
      case x::xs => List(x)
      case Nil => List()
    }
    synth:::types
  }

  def unify(ann: List[AnnotationInfo]): List[AnnotationInfo] = {
    // TODO: what about multiple annotations?
    val synth = ann.filter(a => a.atp.typeSymbol == MarkerCPSSynth) match {
      case x::xs => List(x)
      case Nil => List()
    }
    val types = ann.filter(a => a.atp.typeSymbol == MarkerCPSTypes) match {
      case x::xs => List(x)
      case Nil => List()
    }
    synth:::types
  }


  // anf transform

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

  def updateSynthFlag(tree: Tree) = { // remove annotations if *we* added them (@synth present)
    if (hasSynthAnn(tree.tpe)) {
      log("removing annotation from " + tree)
      tree.setType(tree.tpe.withoutAttributes) // TODO: remove only ours
    } else
      tree
  }

  type CPSInfo = Option[(Type,Type)]

  def linearize(a: CPSInfo, b: CPSInfo): CPSInfo = {
    // TODO: check types
    b orElse a
  }

  
  // cps transform

}