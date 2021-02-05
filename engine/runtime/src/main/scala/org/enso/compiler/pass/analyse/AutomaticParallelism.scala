package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.IRPass

/** This pass is responsible for discovering occurrences of automatically
  * parallelisable computations. If it finds a join point, annotated with the
  * `@Auto_Parallel` annotation, it will discover the incoming computations that
  * can safely be parallelised.
  *
  * This is a limited process, and operates under the following core
  * assumptions:
  *
  * - The incoming edges are _entirely_ separate (this pass will not act on
  *   diamond patterns).
  * - The incoming edges perform no side-effecting computations that would be
  *   observable from the other edges.
  */
object AutomaticParallelism extends IRPass {
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = Configuration
  override val precursorPasses: Seq[IRPass] = Seq(
    AliasAnalysis,
    DataflowAnalysis
  )
  override val invalidatedPasses: Seq[IRPass] = Seq(
    AliasAnalysis,
    DataflowAnalysis
  )

  /** Executes the pass on a module.
    *
    * @param ir the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    *                      to process a module
    *  @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module = ir

  /** Executes the pass on an expression.
    *
    * @param ir the Enso IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    *  @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: IR.Expression,
    inlineContext: InlineContext
  ): IR.Expression = ir

  // TODO [AA] This can probably just work on blocks with assigned metadata.
  //  The metadata would have to contain the traces of the parallel flows.

  // === Pass Configuration ===================================================

  /** The configuration for this pass.
    *
    * @param shouldWriteParallelScopes Whether or not the pass should write
    *                                  parallel scopes into the IR
    */
  case class Configuration(shouldWriteParallelScopes: Boolean)
      extends IRPass.Configuration {
    override var shouldWriteToContext: Boolean = false
  }
}
