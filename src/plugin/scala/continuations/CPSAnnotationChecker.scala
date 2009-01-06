// $Id$

package scala.continuations

import scala.tools.nsc.Global

abstract class CPSAnnotationChecker extends CPSUtils {
  val global: Global
  import global._
  import definitions._

//  override val verbose = true



  /** 
   *  Checks whether @cps annotations conform
   */


  object checker extends AnnotationChecker {

    /** Check annotations to decide whether tpe1 <:< tpe2 */
    def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {

//      vprintln("check annotations: " + tpe1 + " <:< " + tpe2)

      if (tpe1.typeSymbol eq NothingClass)
        return true

      val attribs1 = filterAttribs(tpe1,MarkerCPSTypes)
      val attribs2 = filterAttribs(tpe2,MarkerCPSTypes)

      // HACK: when typing during later phases (e.g. -Ycheck), we need
      // to relax the rules. Otherwise, blocks will get typed <error>
      // before we get a chance to install the correct annotations
      // which would make them pass the ckeck.
      
//      return attribs2.forall(a2 => attribs1.exists(a1 => a1.atp <:< a2.atp))


      // Update: there seem to be errors nonetheless...
      
/*
      if (phase.id <= currentRun.typerPhase.id)
        attribs2.forall(a2 => attribs1.exists(a1 => a1.atp <:< a2.atp))
      else
*/        attribs1.forall(a1 => attribs2.forall(a2 => a1.atp <:< a2.atp))
    }



    
    def updateAttributes(tpe: Type, attribs: List[AnnotationInfo]): Type = {
      tpe match {
        // Need to push attribs into each alternative of overloaded type
        
        // But we can't, since alternatives aren't types but symbols, which we
        // can't change (we'd be affecting symbols globally)
        
        /*
        case OverloadedType(pre, alts) =>
          OverloadedType(pre, alts.map((sym: Symbol) => updateAttributes(pre.memberType(sym), attribs)))
        */

        case _ =>
          // TODO: it's (sort of) a hack for now...
          // for now, don't care about any other attributes that might be present
          if (tpe.attributes.isEmpty) {
            if (!attribs.isEmpty) {
              val synth = MarkerCPSSynth.tpe
              tpe.withAttributes(AnnotationInfo(synth, Nil, Nil)::attribs)
            } else
              tpe
          } else {
            tpe.withoutAttributes.withAttributes(
              unify(tpe.attributes:::attribs))  // TODO: need to consider synth attribute?
          }
      }
    }





    def transArgList(fun: Tree, args: List[Tree]): List[List[Tree]] = {
      val formals = fun.tpe.paramTypes
      val overshoot = args.length - formals.length
    
      for ((a,tp) <- args.zip(formals ::: List.make(overshoot, NoType))) yield {
        tp match {
          case TypeRef(_, sym, List(elemtp)) if sym == ByNameParamClass =>
            Nil // TODO: check conformance??
          case _ =>
            List(a)
        }
      }
    }


    def transStms(stms: List[Tree]): List[Tree] = stms match {
      case ValDef(mods, name, tpt, rhs)::xs =>
        rhs::transStms(xs)
      case x::xs =>
        x::transStms(xs)
      case Nil =>
        Nil
    }


    def transChildrenInOrder(tree: Tree, tpe: Type, childTrees: List[Tree]) = {
      val children = childTrees.flatMap((t:Tree) => filterAttribs(t.tpe, MarkerCPSTypes))
                            
      val newtpe = updateAttributes(tpe, linearize(children))
    
      if (!newtpe.attributes.isEmpty)
        vprintln("[checker] inferred " + tree + " / " + tpe + " ===> "+ newtpe)
      
      newtpe
    }




    /** Modify the type that has thus far been inferred
     *  for a tree.  All this should do is add annotations. */

    override def addAnnotations(tree: Tree, tpe: Type): Type = {

      tree match {

        case Apply(fun @ Select(qual, name), args) if (fun.tpe ne null) && !fun.tpe.isErroneous =>

          // HACK: With overloaded methods, fun will never get annotated. This is because
          // the 'overloaded' type gets annotated, but not the alternatives (among which
          // fun's type is chosen)

          vprintln("[checker] checking select apply " + tree + "/" + tpe)
          
          transChildrenInOrder(tree, tpe, qual::List.flatten(transArgList(fun, args)))

        case Apply(fun, args) if (fun.tpe ne null) && !fun.tpe.isErroneous =>

          vprintln("[checker] checking unknown apply " + tree + "/" + tpe)
          
          transChildrenInOrder(tree, tpe, fun::List.flatten(transArgList(fun, args)))

        case TypeApply(fun, args) =>

          vprintln("[checker] checking type apply " + tree + "/" + tpe)

          transChildrenInOrder(tree, tpe, List(fun))

        case Select(qual, name) =>

          vprintln("[checker] checking select " + tree + "/" + tpe)

          // FIXME: but it back in?? (problem with test cases select.scala and Test2.scala)
          // transChildrenInOrder(tree, tpe, List(qual))
          tpe

        case If(cond, thenp, elsep) =>

          val res = transChildrenInOrder(tree, tpe, List(cond))
          
          // additional requirements:
          // 1) always need a then-part in cps code
          // 2) either then and else must both be cps or none of them
          // for now, leave these checks to SelectiveANF
          
          res

        case Match(select, cases) =>

          transChildrenInOrder(tree, tpe, List(select))

        case Block(stms, expr) =>

          // if any stm has annotation, so does block
          transChildrenInOrder(tree, tpe, transStms(stms))


        case ValDef(mods, name, tpt, rhs) =>
          vprintln("[checker] checking valdef " + name + "/"+tpe+"/"+tpt+"/"+tree.symbol.tpe)

          // ValDef symbols must *not* have annotations!

          if (hasAnswerTypeAnn(tree.symbol.info)) { // is it okay to modify sym here?
            tpt.setType(tpt.tpe.withoutAttributes) // TODO: only remove our attribs
            tree.symbol.setInfo(tree.symbol.info.withoutAttributes)
          }
          tpe

        case _ =>
          tpe
      }

          
    }
  }
}
