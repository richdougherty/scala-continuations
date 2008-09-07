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
abstract class SelectiveCPSTransform extends PluginComponent with InfoTransform with TypingTransformers {
  // inherits abstract value `global' and class `Phase' from Transform

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import typer.{typed,atOwner}           // methods to type trees
  import posAssigner.atPos         // for filling in tree positions 

  /** the following two members override abstract members in Transform */
  val phaseName: String = "selectivecps"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new CPSTransformer(unit)





  lazy val MarkerCPS = definitions.getClass("scala.continuations.cps")
  lazy val MarkerCPSTypes = definitions.getClass("scala.continuations.cpstypes")
  lazy val MarkerUnCPS = definitions.getClass("scala.continuations.uncps")
  lazy val Context = definitions.getClass("scala.continuations.Shift")





/*
  private val cpsTypeMap: TypeMap = new TypeMap {
    def apply(tp: Type): Type = {
      val tp = expandAlias(tp0)
      tp match {
        case MethodType(formals, MethodType(formals1, restpe)) =>
          apply(MethodType(formals ::: formals1, restpe))
        case MethodType(formals, ExistentialType(tparams, restpe @ MethodType(_, _))) =>
          assert(false, "unexpected curried method types with intervening exitential") 
          tp0
        case mt: ImplicitMethodType =>
          apply(MethodType(mt.paramTypes, mt.resultType))
        case PolyType(List(), restpe) =>
          apply(MethodType(List(), restpe))
        case PolyType(tparams, restpe) =>
          PolyType(tparams, apply(MethodType(List(), restpe)))
        case TypeRef(pre, sym, List(arg)) if (sym == ByNameParamClass) =>
          apply(functionType(List(), arg))
        case TypeRef(pre, sym, args) if (sym == RepeatedParamClass) =>
          apply(rawTypeRef(pre, SeqClass, args))
        case _ =>
          expandAlias(mapOver(tp))
      }
    }
  }
*/


    def updateReturnType(tpe: Type, rhs: (Type => Type)): Type = tpe match {
      case PolyType(a,b) => PolyType(a, updateReturnType(b,rhs))
      case MethodType(a,b) => MethodType(a, rhs(b))

      // TODO: more cases?
    }
    
    def updateReturnTypeFromArgs(tpe: Type, rhs: (List[Type] => Type)): Type = 
      tpe match {
      case PolyType(a,b) => PolyType(a, updateReturnTypeFromArgs(b,rhs))
      case MethodType(a,b) => MethodType(a, rhs(a))

      // TODO: more cases?
    }







  def returnTypeForMethod(sym: Symbol)(tp: Type): Type = {
    if (!sym.hasAttribute(MarkerCPS)) 
      return tp
    
    // if the symbol already has a transformed type, choose that
    
    if (tp.typeSymbol == Context)
      return tp

    // otherwise, start with hard-coded defaults

    var res = AnyClass.tpe  // TODO: are these reasonable?
    var outer = AnyClass.tpe


    // look at method attribute and possibly get res from there

    sym.attributes.foreach {
      case AnnotationInfo(an, args, param) if an.typeSymbol == MarkerCPS => 
        res = an.typeArgs(0)
      case _ =>
    }

    // now consider attribute on the return type
  
    tp.attributes.foreach {
      case AnnotationInfo(an, args, param) if an.typeSymbol == MarkerCPSTypes => 
        res = an.typeArgs(0)
        outer = an.typeArgs(1)
      case _ =>
    }
  
    // TODO: correct to remove attributes?
    appliedType(Context.tpe, List(tp.withoutAttributes, res, outer))
  }



  /** - return symbol's transformed type, 
   */
  def transformInfo(sym: Symbol, tp: Type): Type = {
    
    if (sym.isMethod && sym.hasAttribute(MarkerCPS)) {
      
      log("transformInfo called for cps annotated method " + sym + "/" + tp)
      
      updateReturnType(tp, returnTypeForMethod(sym))
      
    } else {
     tp
    }
  }






  class CPSTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    override def transform(tree: Tree): Tree = atPhase(phase.next) { 
      postTransform(mainTransform(tree))
    }


    class ShallowTraverser(maxDepth:Int) extends Traverser {
      var curDepth = 0
      override def traverse(t: Tree) {
        if (curDepth < maxDepth) {
          val saveDepth = curDepth
          curDepth += 1
          super.traverse(t)
          curDepth = saveDepth
        }
      }
    }



    def postTransform(tree: Tree): Tree = {

        // basic idea:
        //
        // Annotated methods will change their signatures. Thus, uses of
        // these methods will also need to be retyped, as well as the
        // enclosing terms the uses occur in.
        //
        // We use a mark&sweep approach: Any term that embodies another
        // term with type null will also get its type set to null.
        // By post-order traversal, we work our way outwards from
        // the terms referencing annotated methods, so we need to
        // only check the directly enclosed terms at each level
        // (avoiding exponential blowup).
        // The sweeping happens at definition sites, where the terms
        // are retyped using localTyper.typed()

        var nullTermsFound = false

        val check = new ShallowTraverser(1) {
          override def traverse(t: Tree) {
            if ((curDepth == 1) && (t.isTerm) && (t.tpe == null)) {
  //          if (curDepth == 1 && t.tpe == null) {
              log("found null-type tree " + t)
              nullTermsFound = true
            }
            super.traverse(t);
          }
        }
      
        check(tree)
      
        if (tree.isTerm && tree.tpe != null) {
          if (nullTermsFound) {
            log("will reset type for " + tree + " / " + tree.tpe)
            tree.tpe = null
          }
          tree
        } else if (tree.isDef) {
          if (nullTermsFound) {
            log("will re-type def " + tree + " / " + tree.tpe)
            tree.tpe = null
            localTyper.typed(tree)
          } else {
            tree
          }
        } else {
          tree
        }
        
    }
    





    def mainTransform(tree: Tree): Tree = {
      tree match {

        case Apply(fun, expr)
        if (tree.symbol.hasAttribute(MarkerUnCPS)) =>  //just remove them!
          // TODO: assert expr.length == 1
          transform(expr(0))

        case Ident(_) | Select(_, _)
        if (tree.symbol.hasAttribute(MarkerCPS)) =>
          log("translating ref to method: " + tree)
          super.transform(tree).setType(null) // mark term for re-typing


        case dd @ DefDef(mods, name, tparams, vparams, tpt, rhs)
        if (tree.symbol.hasAttribute(MarkerCPS)) =>

          log("transforming method: " + tree.symbol.fullNameString)

          val methTpe = returnTypeForMethod(dd.symbol)(tpt.tpe)

//          val rhs1 = atOwner(dd.symbol) { localTyper.typed(resetAttrs(transform(rhs))) }
//          val rhs1 =  atOwner(dd.symbol) { localTyper.typed(transform(rhs)) }
          val rhs1 =  atOwner(dd.symbol) { localTyper.typed(transform(rhs)) }

//          log("result (of "+dd.symbol+"): "+rhs1)
          log("result is of type "+rhs1.tpe)
          log("method symbol is of type "+dd.symbol.tpe)
          log("method tpt "+tpt)

          val expect = if (dd.symbol.isConstructor) UnitClass.tpe else methTpe

          if (!(rhs1.tpe <:< expect)) {
            // TODO: is there a more general way to present a type error?
            unit.warning(rhs.pos, "right hand side of type "+rhs1.tpe+
                " does not conform to declared return type "+expect)
          }

	        copy.DefDef(dd, mods, name, tparams, vparams, TypeTree(methTpe).setPos(tpt.pos), rhs1) 

        case Block(stms, expr) => 
        
          // TODO: should we require to be in a @cps annotated method?
	        val (stms1, expr1) = transBlock(stms, expr)
          copy.Block(tree, stms1, expr1).setType(expr1.tpe)

        case _ => 
          super.transform(tree)//.setType(null)
      }
    }



    def transBlock(stms: List[Tree], expr: Tree): (List[Tree], Tree) = {

      stms match {
        case Nil =>
          (Nil, transform(expr))

        case stm::rest =>

          stm match {
            case vd @ ValDef(mods, name, tpt, rhs)
            if (vd.symbol.hasAttribute(MarkerCPS)) =>

              log("found marked ValDef "+name+" of type " + vd.symbol.tpe)

      	      val tpe = vd.symbol.tpe

//  	        val rhs1 = transform(rhs)
              val rhs1 = localTyper.typed(transform(rhs))
//  	        val rhs1 = localTyper.typed(resetAttrs(transform(rhs))) // FIXME: can do without resetAttrs?

      	      log("valdef symbol " + vd.symbol + " has type " + tpe)
      	      log("right hand side " + rhs1 + " has type " + rhs1.tpe)

      	      log("currentOwner: " + currentOwner)
      	      log("currentMethod: " + currentMethod)


//	          val cls = currentMethod.newAnonymousFunctionClass(rhs.pos)
//            val sym = currentMethod.newValue(rhs.pos)
//            val arg = sym.newValueParameter(rhs.pos, name).setInfo(tpe)
              val arg = currentMethod.newValueParameter(rhs.pos, name).setInfo(tpe)

              val body = {
                val (a, b) = transBlock(rest, expr)
//                resetAttrs(Block(a, b)) // FIXME: can we do without?
                Block(a, b)
              }
              
              new TreeSymSubstituter(List(vd.symbol), List(arg)).traverse(body)


//              // FIXME: don't know last parameter (result type of shift's body)
//              val stpe = appliedType(Context.tpe, List(tpe))

//              log("computed shift will have type " + stpe)

//              new ChangeOwnerTraverser(currentMethod, sym).traverse(body)
//              new TreeSymSubstituter(List(vd.symbol), List(arg)).traverse(body)
              
              val fun = localTyper.typed(Function(
                List(
                  ValDef(arg)
                ),
                body
              ))// setSymbol(sym)
              
//            sym.setInfo(fun.tpe) // TODO: not needed?
              

	            val sym = fun.symbol
	            arg.owner = sym

              new ChangeOwnerTraverser(currentMethod, sym).traverse(body)


      	      log("fun.symbol: "+fun.symbol)
      	      log("fun.symbol.owner: "+fun.symbol.owner)
      	      log("arg.owner: "+arg.owner)

              
              log("fun.tpe:"+fun.tpe)
              log("return type of fun:"+body.tpe)
              
              var methodName = "map"
              
              // FIXME: better reporting of type errors?
              
              if (body.tpe != null) {
                if (body.tpe.typeSymbol.tpe <:< Context.tpe)
                  methodName = "flatMap"
              }
              else
                unit.error(rhs.pos, "cannot compute type for CPS-transformed function result")
              
              log("will use method:"+methodName)
              
              val applied = atPos(vd.symbol.pos) { localTyper.typed(Apply(
                Select(
		              rhs1,
                  rhs1.tpe.member(methodName)
                ),
                List(
                  fun
                )
              ))}
              
              (Nil, applied)

            case _ => 
                val (a, b) = transBlock(rest, expr)
                (transform(stm)::a, b)
            }
      }
    }


  }
}
