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
  import typer.{atOwner}           // methods to type trees
  import posAssigner.atPos         // for filling in tree positions 

  /** the following two members override abstract members in Transform */
  val phaseName: String = "selectivecps"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new CPSTransformer(unit)





  lazy val MarkerCPS = definitions.getClass("scala.continuations.cps")
  lazy val MarkerUnCPS = definitions.getClass("scala.continuations.uncps")
  lazy val Context = definitions.getClass("scala.continuations.Context")





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


    def getReturnType(tpe: Type): Type = tpe match {
      case PolyType(a,b) => getReturnType(b)
      case MethodType(a,b) => b

      // FIXME: more cases?
    }

    def updateReturnType(tpe: Type, rhs: (Type => Type)): Type = tpe match {
      case PolyType(a,b) => PolyType(a, updateReturnType(b,rhs))
      case MethodType(a,b) => MethodType(a, rhs(b))

      // FIXME: more cases?
    }
    
    def updateReturnTypeFromArgs(tpe: Type, rhs: (List[Type] => Type)): Type = 
      tpe match {
      case PolyType(a,b) => PolyType(a, updateReturnTypeFromArgs(b,rhs))
      case MethodType(a,b) => MethodType(a, rhs(a))

      // FIXME: more cases?
    }







  /** - return symbol's transformed type, 
   */
  def transformInfo(sym: Symbol, tp: Type): Type = {
    
    if (sym.isMethod && sym.hasAttribute(MarkerCPS)) {
      
      log("transformInfo called for cps annotated method " + sym + "/" + tp)
      
      updateReturnType(tp, (r:Type) => appliedType(Context.tpe, List(r)))
      
    } else if (sym.isMethod && sym.hasAttribute(MarkerUnCPS)) {

      log("transformInfo called for uncps annotated method " + sym + "/" + tp)
      
      updateReturnTypeFromArgs(tp, (r:List[Type]) => r(0))

    } else if (sym.isValue && sym.hasAttribute(MarkerCPS)) {

      log("transformInfo called for cps annotated value " + sym + "/" + tp)

      tp

    } else if (sym.isValue && sym.hasAttribute(MarkerUnCPS)) {

      log("transformInfo called for uncps annotated value " + sym + "/" + tp)

      appliedType(Context.tpe, List(tp))
    } else {
     tp
    }
  }






  class CPSTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    override def transform(tree: Tree): Tree = atPhase(phase.next) {
      tree match {

        case dd @ DefDef(mods, name, tparams, vparams, tpt, rhs)
        if (dd.symbol.hasAttribute(MarkerUnCPS)) =>

          log("transforming to id function: " + tree.symbol.fullNameString)

          val rhs1 =  atOwner(dd.symbol) {
             vparams match {
               case List(List(vd @ ValDef(mods, name, tpt, _))) =>
                localTyper.typed(Ident(vd.symbol))
             }
          }

          log("result "+rhs1)
          log("result is of type "+rhs1.tpe)
          log("method symbol was of type "+dd.symbol.tpe)

	        copy.DefDef(dd, mods, name, tparams, vparams, tpt, rhs1)
	  
        case Block(stms, expr) => 
        
          // TODO: should we require to be in a @cps annotated method?
        
	        val (stms1, expr1) = transBlock(stms, expr)
          copy.Block(tree, stms1, expr1)

/*
        case dd @ DefDef(mods, name, tparams, vparams, tpt, rhs)
        if (dd.symbol.hasAttribute(MarkerCPS)) =>

          log("transforming " + tree.symbol.fullNameString)

          val rhs1 = atOwner(dd.symbol) {
              val (stms1, expr1) = rhs match {
              case Block(stms, expr) => transBlock(stms, expr)
              case expr => transBlock(Nil, expr)
            }
            localTyper.typed(Block(stms1, expr1))
          }

          log("result "+rhs1)
          log("result is of type "+rhs1.tpe)
          log("method symbol was of type "+dd.symbol.tpe)
          
// *
          def updateReturnType(tpe: Type, rhs: Type): Type = tpe match {
            case PolyType(a,b) => PolyType(a, updateReturnType(b,rhs))
            case MethodType(a,b) => MethodType(a, rhs)

            // FIXME: more cases?
          }
// * /

 //         dd.symbol.updateInfo(updateReturnType(dd.symbol.tpe, rhs1.tpe))

//          log("method symbol updated to type "+dd.symbol.tpe)

          copy.DefDef(dd, mods, name, tparams, vparams, 
            TypeTree(rhs1.tpe), rhs1)

*/
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
            if (vd.symbol.hasAttribute(MarkerUnCPS)) =>
              
              log("found negative ValDef "+name+" of type " + vd.symbol.tpe)

              // TODO: needed at all?

              val (a, b) = transBlock(rest, expr)
              (transform(stm)::a, b)
              
            case vd @ ValDef(mods, name, tpt, rhs)
            if (vd.symbol.hasAttribute(MarkerCPS)) =>

              log("found marked ValDef "+name+" of type " + vd.symbol.tpe)

      	      val tpe = rhs.tpe

      	      val rhs1 = localTyper.typed(resetAttrs(rhs))

      	      log("right hand side " + rhs1 + " has type " + rhs1.tpe)

      	      log("currentOwner: " + currentOwner)
      	      log("currentMethod: " + currentMethod)


//	          val cls = currentMethod.newAnonymousFunctionClass(rhs.pos)
//            val sym = currentMethod.newValue(rhs.pos)
//            val arg = sym.newValueParameter(rhs.pos, name).setInfo(tpe)
              val arg = currentMethod.newValueParameter(rhs.pos, name).setInfo(tpe)

              val body = {
                val (a, b) = transBlock(rest, expr)
                resetAttrs(Block(a, b))
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
              

//	          val sym = fun.symbol

//	          arg.owner = sym

//            new ChangeOwnerTraverser(currentMethod, sym).traverse(fun)
//	          sym.owner = currentMethod

      	      log("fun.symbol: "+fun.symbol)
      	      log("fun.symbol.owner: "+fun.symbol.owner)
      	      log("arg.owner: "+arg.owner)

              
              log("fun.tpe:"+fun.tpe)
              log("return type of fun:"+body.tpe)
              
              var methodName = "map"
              
              if (body.tpe != null) {
                if (isSubType(body.tpe.typeSymbol.tpe, Context.tpe))
                  methodName = "flatMap"
              }
              else
                error("Cannot compute type for transformed function result")
              
              log("will use method:"+methodName)
              
              val applied = localTyper.typed(Apply(
                Select(
		              rhs1,
                  rhs1.tpe.member(methodName)
                ),
                List(
                  fun
                )
              ))
              
              (Nil, applied)

            case _ => 
                val (a, b) = transBlock(rest, expr)
                (transform(stm)::a, b)
            }
      }
    }


  }
}
