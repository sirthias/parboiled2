package org.parboiled2

import org.parboiled2.support._
import shapeless.HList

/**
 * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
 */
object Macros {
  import scala.reflect.macros.blackbox.Context

  type Context1[A] = Context { type PrefixType = Parser#RuleCreator1[A] }
  type Context2[A, B] = Context { type PrefixType = Parser#RuleCreator2[A, B] }
  type Context3[A, B, C] = Context { type PrefixType = Parser#RuleCreator3[A, B, C] }

  def rule0[Ctx: ctx.WeakTypeTag, I <: HList: ctx.WeakTypeTag, O <: HList: ctx.WeakTypeTag](ctx: Context)(
    r: ctx.Expr[Rule[Ctx, I, O]]): ctx.Tree = rule0Impl(ctx)(r, debug = false)

  def debugRule0[Ctx: ctx.WeakTypeTag, I <: HList: ctx.WeakTypeTag, O <: HList: ctx.WeakTypeTag](ctx: Context)(
    r: ctx.Expr[Rule[Ctx, I, O]]): ctx.Tree = rule0Impl(ctx)(r, debug = true)

  private def rule0Impl[Ctx: ctx.WeakTypeTag, I <: HList: ctx.WeakTypeTag, O <: HList: ctx.WeakTypeTag](ctx: Context)(
    r: ctx.Expr[Rule[Ctx, I, O]], debug: Boolean): ctx.Tree = {
    import ctx.universe._
    val opTreeCtx = new OpTreeContext[ctx.type](ctx)
    import opTreeCtx._
    tpeCtx = weakTypeOf[Ctx]
    val body = ResultExpression(r.tree) mapStatements StateAccessTransformer mapResultAndGet { tree ⇒
      val opTree = RuleCall(OpTreeCall(OpTree(tree)), ruleName(ctx))
      q"""
      def wrapped: Boolean = ${opTree.render(wrapped = true)}
      if (__psi.inErrorAnalysis) wrapped else ${opTree.render(wrapped = false)}"""
    }
    val tree = q"""
      new $prefix.RuleImpl[$tpeCtx] {
        def run(__psi: $prefix.ParserStateImpl[$tpeCtx]): Boolean = $body
      }.asInstanceOf[$prefix.Rule[$tpeCtx, ${weakTypeOf[I]}, ${weakTypeOf[O]}]]"""
    if (debug) println(tree)
    tree
  }

  def rule1[Ctx: ctx.WeakTypeTag, A: ctx.WeakTypeTag, I <: HList: ctx.WeakTypeTag, O <: HList: ctx.WeakTypeTag](
    ctx: Context1[A])(r: ctx.Expr[A ⇒ Rule[Ctx, I, O]]): ctx.Tree = {
    import ctx.universe._
    val opTreeCtx = new OpTreeContext[ctx.type](ctx)
    import opTreeCtx._
    tpeCtx = weakTypeOf[Ctx]
    ResultExpression(r.tree) mapResultAndGet { tree ⇒
      ctx.untypecheck(tree) match {
        case Function(List(ValDef(_, argName, _, _)), body) ⇒
          val runBody = ResultExpression(body) mapStatements StateAccessTransformer mapResultAndGet { bodyTree ⇒
            val opTree = RuleCall(OpTreeCall(OpTree(bodyTree)), ruleName(ctx))
            q"""
            def wrapped: Boolean = ${opTree.render(wrapped = true)}
            if (__psi.inErrorAnalysis) wrapped else ${opTree.render(wrapped = false)}"""
          }
          val tpeCtx = weakTypeOf[Ctx]
          val tpeA = weakTypeOf[A]
          q"""
          new $prefix.Rule1XImpl[$tpeCtx, $tpeA] {
            def run($argName: $tpeA, __psi: $prefix.ParserStateImpl[$tpeCtx]): Boolean = $runBody
          }.asInstanceOf[$prefix.Rule1X[$tpeCtx, $tpeA, ${weakTypeOf[I]}, ${weakTypeOf[O]}]]"""
        case x ⇒ ctx.abort(x.pos, "Expression must be a Function1 literal")
      }
    }
  }

  def rule2[Ctx: ctx.WeakTypeTag, A: ctx.WeakTypeTag, B: ctx.WeakTypeTag, I <: HList: ctx.WeakTypeTag, O <: HList: ctx.WeakTypeTag](
    ctx: Context2[A, B])(r: ctx.Expr[(A, B) ⇒ Rule[Ctx, I, O]]): ctx.Tree = {
    import ctx.universe._
    val opTreeCtx = new OpTreeContext[ctx.type](ctx)
    import opTreeCtx._
    tpeCtx = weakTypeOf[Ctx]
    ResultExpression(r.tree) mapResultAndGet { tree ⇒
      ctx.untypecheck(tree) match {
        case Function(List(ValDef(_, arg1Name, _, _), ValDef(_, arg2Name, _, _)), body) ⇒
          val runBody = ResultExpression(body) mapStatements StateAccessTransformer mapResultAndGet { bodyTree ⇒
            val opTree = RuleCall(OpTreeCall(OpTree(bodyTree)), ruleName(ctx))
            q"""
            def wrapped: Boolean = ${opTree.render(wrapped = true)}
            if (__psi.inErrorAnalysis) wrapped else ${opTree.render(wrapped = false)}"""
          }
          val tpeCtx = weakTypeOf[Ctx]
          val tpeA = weakTypeOf[A]
          val tpeB = weakTypeOf[B]
          q"""
          new $prefix.Rule2XImpl[$tpeCtx, $tpeA, $tpeB] {
            def run($arg1Name: $tpeA, $arg2Name: $tpeB, __psi: $prefix.ParserStateImpl[$tpeCtx]): Boolean = $runBody
          }.asInstanceOf[$prefix.Rule2X[$tpeCtx, $tpeA, $tpeB, ${weakTypeOf[I]}, ${weakTypeOf[O]}]]"""
        case x ⇒ ctx.abort(x.pos, "Expression must be a Function2 literal")
      }
    }
  }

  def rule3[Ctx: ctx.WeakTypeTag, A: ctx.WeakTypeTag, B: ctx.WeakTypeTag, C: ctx.WeakTypeTag, I <: HList: ctx.WeakTypeTag, O <: HList: ctx.WeakTypeTag](
    ctx: Context3[A, B, C])(r: ctx.Expr[(A, B, C) ⇒ Rule[Ctx, I, O]]): ctx.Tree = {
    import ctx.universe._
    val opTreeCtx = new OpTreeContext[ctx.type](ctx)
    import opTreeCtx._
    tpeCtx = weakTypeOf[Ctx]
    ResultExpression(r.tree) mapResultAndGet { tree ⇒
      ctx.untypecheck(tree) match {
        case Function(List(ValDef(_, arg1Name, _, _), ValDef(_, arg2Name, _, _), ValDef(_, arg3Name, _, _)), body) ⇒
          val runBody = ResultExpression(body) mapStatements StateAccessTransformer mapResultAndGet { bodyTree ⇒
            val opTree = RuleCall(OpTreeCall(OpTree(bodyTree)), ruleName(ctx))
            q"""
            def wrapped: Boolean = ${opTree.render(wrapped = true)}
            if (__psi.inErrorAnalysis) wrapped else ${opTree.render(wrapped = false)}"""
          }
          val tpeCtx = weakTypeOf[Ctx]
          val tpeA = weakTypeOf[A]
          val tpeB = weakTypeOf[B]
          val tpeC = weakTypeOf[C]
          q"""
          new $prefix.Rule3XImpl[$tpeCtx, $tpeA, $tpeB, $tpeC] {
            def run($arg1Name: $tpeA, $arg2Name: $tpeB, $arg3Name: $tpeC,
              __psi: $prefix.ParserStateImpl[$tpeCtx]): Boolean = $runBody
          }.asInstanceOf[$prefix.Rule3X[$tpeCtx, $tpeA, $tpeB, $tpeC, ${weakTypeOf[I]}, ${weakTypeOf[O]}]]"""
        case x ⇒ ctx.abort(x.pos, "Expression must be a Function3 literal")
      }
    }
  }

  private def ruleName(ctx: Context): ctx.Tree = {
    import ctx.universe._
    q"""
    val __name = ${ctx.prefix}.name
    if (__name.isEmpty) ${ctx.internal.enclosingOwner.name.decodedName.toString.trim} else __name"""
  }
}
