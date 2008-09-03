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
  lazy val MarkerUnCPS = definitions.getClass("scala.continuations.uncps")
  lazy val Shift = definitions.getClass("scala.continuations.Shift")

  lazy val cpsModule = definitions.getMember(definitions.getModule("scala.continuations"), "CPS")
  lazy val shift2val = definitions.getMember(cpsModule, "shift2val")  
  lazy val val2shift = definitions.getMember(cpsModule, "val2shift")


  class ANFTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    override def transform(tree: Tree): Tree = {
      tree match {
        
        case dd @ DefDef(mods, name, tparams, vparams, tpt, rhs) =>
//        if (dd.symbol.hasAttribute(MarkerCPS)) =>

          log("transforming " + tree.symbol.fullNameString)

          val rhs1 = atOwner(dd.symbol) {
              transExpr(rhs)
          }

          log("result "+rhs1)
          log("result is of type "+rhs1.tpe)

          copy.DefDef(dd, mods, name, tparams, vparams,
            tpt, rhs1)

        case _ => 
          super.transform(tree)
      }
    }



    def getQualifiedSymbol(tree: Tree): Symbol = {

      // need to find the "real" method symbol (-> annotations)
      // TODO: is there a standard way of doing this?

      tree match {
        case Select(qual, name) =>
          
          val par = getQualifiedSymbol(qual)
          if (par != null && par.tpe != null) {
            par.tpe.member(name)
          } else
            tree.symbol
        case TypeApply(fun, args) =>
          getQualifiedSymbol(fun)
        case _ => tree.symbol

        // TODO: other cases?
      }
    }

    def transExpr(tree: Tree): Tree = {
      transTailExpr(tree) match {
        case (Nil, b) => b
        case (a, b) =>
          copy.Block(tree, a,b)
      }
    }


    def transParamList(fun: Tree, args: List[Tree]): (List[List[Tree]], List[Tree]) = {
      val formals:List[Type] = fun.tpe match {
        case MethodType(params, result) => params
      }
    
      val overshoot = args.length - formals.length
    
      List.unzip(for ((a,tp) <- args.zip(formals ++ List.make(overshoot, NoType))) yield {
        // log tp.typeSymbol
        if (tp.typeSymbol == ByNameParamClass)
          (Nil, transExpr(a))
        else
          transInlineExpr(a)
      })
    }


    def transTailExpr(tree: Tree): (List[Tree], Tree) = {
      tree match {
        case Block(stms, expr) => 
          val (a, b) = transBlock(stms, expr)
          (Nil, copy.Block(tree, a, b))

        case ValDef(mods, name, tptTre, rhs) =>
          val (stms, anfRhs) = transInlineExpr(rhs)
          (stms, copy.ValDef(tree, mods, name, tptTre, anfRhs))

        // FIXME: if valdef is already marked, don't translate it again
          
        case Select(qual, name) =>
          val (stms, expr) = transInlineExpr(qual)
          (stms, copy.Select(tree, expr, name))

        case If(cond, thenp, elsep) =>
          val (condStats, condVal) = transInlineExpr(cond)
          val thenVal = transExpr(thenp)
          val elseVal = transExpr(elsep)

          (condStats, copy.If(tree, condVal, thenVal, elseVal))

        case TypeApply(fun, args) =>
          val (stms, expr) = transInlineExpr(fun)
          (stms, copy.TypeApply(tree, expr, args))


        case Apply(fun, args) =>

//          if (getQualifiedSymbol(fun).hasAttribute(MarkerUnCPS)) {
//            transTailExpr(args(0))


            val (funStm, funExpr) = transInlineExpr(fun)
            val (argStm, argExpr) = transParamList(fun, args)

            (funStm ::: List.flatten(argStm), copy.Apply(tree, funExpr, argExpr))

        case _ =>
          (Nil, transform(tree))
      }
    }
    
    
    def transInlineExpr(tree: Tree): (List[Tree], Tree) = {
      tree match {

        case Apply(fun, args)
        if (getQualifiedSymbol(fun).hasAttribute(MarkerUnCPS)) =>

          val (funStm, funExpr) = transInlineExpr(fun)
          val (argStm, argExpr) = transParamList(fun, args)

          val valueTpe = tree.tpe

          val sym = currentOwner.newValue(tree.pos, unit.fresh.newName(tree.pos, "tmp"))
                      .setInfo(valueTpe)
                      .setFlag(Flags.SYNTHETIC)
                      .setAttributes(List(AnnotationInfo(MarkerCPS.tpe, Nil, Nil)))        

          (funStm ::: List.flatten(argStm) ::: 
             List(ValDef(sym, copy.Apply(tree, funExpr, argExpr)) setType(NoType)),
             Ident(sym) setType(valueTpe) setPos(tree.pos))          


        case Apply(fun, args)
        if (getQualifiedSymbol(fun).hasAttribute(MarkerCPS)) =>
        
          if (!currentMethod.hasAttribute(MarkerCPS)) {// cps transform phase currently depends on this
            unit.error(tree.pos, "cannot call cps-transformed function in a value position "+
              "in a method that is not transformed")

            return (Nil, tree)
          }
        
          val (funStm, funExpr) = transInlineExpr(fun)
          val (argStm, argExpr) = transParamList(fun, args)
          
          log("found apply of "+fun+", which is marked")
          log("result of application has type " + tree.tpe)
          
          val valueTpe = tree.tpe

          val sym = currentOwner.newValue(tree.pos, unit.fresh.newName(tree.pos, "tmp"))
                      .setInfo(valueTpe)
                      .setFlag(Flags.SYNTHETIC)
                      .setAttributes(List(AnnotationInfo(MarkerCPS.tpe, Nil, Nil)))        

          (funStm ::: List.flatten(argStm) ::: 
             List(ValDef(sym, copy.Apply(tree, funExpr, argExpr)) setType(NoType)),
             Ident(sym) setType(valueTpe) setPos(tree.pos))

        case _ =>
          transTailExpr(tree)
      }
    }


    def transBlock(stms: List[Tree], expr: Tree): (List[Tree], Tree) = {

      stms match {
        case Nil =>
          transTailExpr(expr)

        case stm::rest =>
          val (headStm, headExpr) = transInlineExpr(stm)
          val (restStm, restExpr) = transBlock(rest, expr)
          (headStm:::List(headExpr):::restStm, restExpr)
       }
    }


  }
}
