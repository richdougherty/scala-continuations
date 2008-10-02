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
        
        case dd @ DefDef(mods, name, tparams, vparamss, tpt, rhs)
	  if ((mods.flags != 1073742368)) => // HACK!!!! (case classes problem, see Test3)
//        if (dd.symbol.hasAttribute(MarkerCPS)) =>

          log("transforming " + dd.symbol)

          atOwner(dd.symbol) {
            val rhs1 = transExpr(rhs, hasAnn(tpt.tpe))
      
            log("result "+rhs1)
            log("result is of type "+rhs1.tpe)

            copy.DefDef(dd, mods, name, transformTypeDefs(tparams), transformValDefss(vparamss),
			transform(tpt), rhs1)
	  }

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

        case Select(qual, name) =>
          val (stms, expr, spc) = transInlineValue(qual)
          (stms, copy.Select(tree, expr, name), spc)
/*	
	case EmptyTree =>
	  if (cps.isDefined)
	    (Nil, atPos(tree.pos) { Literal(()).setType(UnitClass.tpe) }, None)
	  else
	    (Nil, tree, None)
 */
	case If(cond, thenp, elsep) =>
          val (condStats, condVal, spc) = transInlineValue(cond)
          val thenVal = transExpr(thenp, cps)
	  val elseVal = transExpr(elsep, cps)
//          val elseVal = transExpr(if (cps.isDefined && elsep == EmptyTree)
//	    Literal(()).setType(UnitClass.tpe).setPos(tree.pos) else elsep, cps)
	
          // TODO: check that then and else parts agree
	  
	  if (cps.isDefined) {
	    if (elsep == EmptyTree)
	      unit.error(tree.pos, "always need else part in cps code")
	  }
	  
	  if (!(hasAnn(thenVal.tpe).isDefined == hasAnn(elseVal.tpe).isDefined)) {
	      unit.error(tree.pos, "then and else part must both be cps code or neither of them")
	  }

          (condStats, copy.If(tree, condVal, thenVal, elseVal).setType(thenVal.tpe), cps)

	  // FIXME: type of else part not reflected

	case LabelDef(name, params, rhs) =>
          val rhsVal = transExpr(rhs, cps)

	  // FIXME: ref to declared label still carries old type
	
	  if (hasAnn(rhsVal.tpe).isDefined) {
	    unit.error(tree.pos, "cps code not (yet) allowed in while loops")
	  }
	  
	  (Nil, copy.LabelDef(tree, name, params, rhsVal).setType(rhsVal.tpe), cps)
	  
	
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

      if ((cps.isDefined || spc.isDefined) && !(hasAnn(expr.tpe).isDefined || expr.isInstanceOf[Throw] || expr == EmptyTree)) {
        log("cps type error (expected: " + cps + "/"+ spc + ") in: " + expr)
	  
        try {

	  val Some((a, b)) = cps orElse spc
	  
          val res = localTyper.typed(atPos(tree.pos) {
	          Apply(TypeApply(gen.mkAttributedRef(MethShiftUnit), 
                    List(TypeTree(expr.tpe), TypeTree(a), TypeTree(b))), List(expr))
          })

          (stms, res) // TODO: apply conversion
	
	} catch {
	  case ex:TypeError =>
	    unit.error(ex.pos, "cannot cps-transform expression " + tree + ": " + ex.msg)
	    (stms, expr)
	}

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

    }


    def transBlock(stms: List[Tree], expr: Tree, cps: Option[(Type,Type)]): (List[Tree], Tree) = {
      stms match {
        case Nil =>
          transTailValue(expr, cps)

        case stm::rest =>
	  var (rest2, expr2) = (rest, expr)
          val (headStms, headSpc) = stm match {

	    // TODO: what about DefDefs?

            case tree@ValDef(mods, name, tptTre, rhs) =>
              val (stms, anfRhs, spc) = atOwner(tree.symbol) { transInlineValue(rhs) }
	    
              val tv = new ChangeOwnerTraverser(tree.symbol, currentOwner)
              stms.foreach(tv.traverse(_))

	      if (spc.isDefined && anfRhs.isInstanceOf[Ident]) { // FIXME: right condition?

		// TODO: exponential. not sooo good.

		val tv2 = new TreeSubstituter(List(tree.symbol), List(anfRhs))
		rest2 = rest.map(tv2.transform(_))
		expr2 = tv2.transform(expr)
		
		// TODO: in fact, wer're removing any user-given type in the
		// valdef. problem?

		(stms, spc)
	      } else {
		(stms:::List(copy.ValDef(tree, mods, name, tptTre, anfRhs)), spc)
	      }


	    case _ =>
	      val (headStms, headExpr, headSpc) = transInlineValue(stm)
	      (headStms:::List(headExpr), headSpc)
	  }
          val (restStms, restExpr) = transBlock(rest2, expr2, cps orElse headSpc)
          (headStms:::restStms, restExpr)
       }
    }


  }
}
