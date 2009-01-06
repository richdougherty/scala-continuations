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
abstract class SelectiveCPSTransform extends PluginComponent with 
  InfoTransform with TypingTransformers with CPSUtils {
  // inherits abstract value `global' and class `Phase' from Transform

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import typer.atOwner             // methods to type trees
  import posAssigner.atPos         // for filling in tree positions 

  /** the following two members override abstract members in Transform */
  val phaseName: String = "selectivecps"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new CPSTransformer(unit)



  /** - return symbol's transformed type, 
   */
  def transformInfo(sym: Symbol, tp: Type): Type = {

    val newtp = transformCPSType(tp)

    if (newtp != tp)
      log("transformInfo changed type for " + sym + " to " + newtp);

    if (sym == MethReifyR)
      log("transformInfo (not)changed type for " + sym + " to " + newtp);

    newtp
  }

  def transformCPSType(tp: Type): Type = {  // TODO: use a TypeMap? handle more cases?
    tp match {
      case PolyType(params,res) => PolyType(params, transformCPSType(res))
      case MethodType(params,res) => MethodType(params.map(transformCPSType(_)), transformCPSType(res))
      case TypeRef(pre, sym, args) => TypeRef(pre, sym, args.map(transformCPSType(_)))
      case _ =>
        getAnswerTypeAnn(tp) match {
          case Some((res, outer)) =>
            appliedType(Context.tpe, List(tp.withoutAttributes, res, outer))
          case _ =>
            tp
        }
    }
  }


  class CPSTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    override def transform(tree: Tree): Tree = {
      postTransform(mainTransform(tree))
    }

    def postTransform(tree: Tree): Tree = {
      tree.setType(transformCPSType(tree.tpe))
    }


    def mainTransform(tree: Tree): Tree = {
      tree match {

        case Apply(TypeApply(fun, targs), args)
        if (fun.symbol == MethShift) =>
          log("found shift: " + tree)
          atPos(tree.pos) {
            val funR = gen.mkAttributedRef(MethShiftR) // TODO: correct?
            log(funR.tpe)
            Apply(
                TypeApply(funR, targs).setType(appliedType(funR.tpe, targs.map((t:Tree) => t.tpe))),
                args.map(transform(_))
            ).setType(transformCPSType(tree.tpe))
          }
        
        case Apply(TypeApply(fun, targs), args)
        if (fun.symbol == MethShiftUnit) =>
          log("found shiftUnit: " + tree)
          atPos(tree.pos) {
            val funR = gen.mkAttributedRef(MethShiftUnitR) // TODO: correct?
            log(funR.tpe)
            Apply(
                TypeApply(funR, List(targs(0), targs(1))).setType(appliedType(funR.tpe, 
                    List(targs(0).tpe, targs(1).tpe))),
                args.map(transform(_))
            ).setType(appliedType(Context.tpe, List(targs(0).tpe,targs(1).tpe,targs(1).tpe)))
          }

        case Apply(TypeApply(fun, targs), args)
        if (fun.symbol == MethReify) =>
          log("found reify: " + tree)
          atPos(tree.pos) {
            val funR = gen.mkAttributedRef(MethReifyR) // TODO: correct?
            log(funR.tpe)
            Apply(
                TypeApply(funR, targs).setType(appliedType(funR.tpe, targs.map((t:Tree) => t.tpe))),
                args.map(transform(_))
            ).setType(transformCPSType(tree.tpe))
          }

        case Block(stms, expr) => 
        
          val (stms1, expr1) = transBlock(stms, expr)
          copy.Block(tree, stms1, expr1)

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
            if (vd.symbol.hasAttribute(MarkerCPS)) =>

              log("found marked ValDef "+name+" of type " + vd.symbol.tpe)

              val tpe = vd.symbol.tpe

              val rhs1 = transform(rhs)

              log("valdef symbol " + vd.symbol + " has type " + tpe)
              log("right hand side " + rhs1 + " has type " + rhs1.tpe)

              log("currentOwner: " + currentOwner)
              log("currentMethod: " + currentMethod)


//            val cls = currentMethod.newAnonymousFunctionClass(rhs.pos)
//            val sym = currentMethod.newValue(rhs.pos)
//            val arg = sym.newValueParameter(rhs.pos, name).setInfo(tpe)
              val arg = currentOwner.newValueParameter(rhs.pos, name).setInfo(tpe)

              val body = {
                val (a, b) = transBlock(rest, expr)
                Block(a, b)
              }
              
              new TreeSymSubstituter(List(vd.symbol), List(arg)).traverse(body)

              val fun = localTyper.typed(Function(
                List(
                  ValDef(arg)
                ),
                body
              ))// setSymbol(sym)
              
//            sym.setInfo(fun.tpe) // TODO: not needed?
              

              val sym = fun.symbol
              arg.owner = sym

              new ChangeOwnerTraverser(currentOwner, sym).traverse(body)


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
              
              try {
                
                val applied = localTyper.typed(atPos(vd.symbol.pos) {
                  Apply(
                    Select(
                      rhs1,
                      rhs1.tpe.member(methodName)
                    ),
                    List(
                      fun
                    )
                  )
                })
              
                (Nil, applied)

              } catch {
                case ex:TypeError =>
                  unit.error(ex.pos, ex.msg)
                  Block.unapply(body).get
              }

            case _ => 
                val (a, b) = transBlock(rest, expr)
                (transform(stm)::a, b)
            }
      }
    }


  }
}
