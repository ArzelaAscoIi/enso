package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Definition
import org.enso.compiler.core.IR.{Error, Name, Type}
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.desugar.ComplexType

import scala.annotation.unused

/** This pass is responsible for discovering occurrences of automatically
  * parallelizable computations. If it finds a join point, annotated with the
  * &#96;@Auto_Parallel&#96; annotation, it will discover the incoming
  * computations that can safely be parallelized.
  *
  * This is a limited process, and operates under the following core
  * assumptions:
  *
  * - The incoming edges are _entirely_ separate (this pass will not act on
  *   diamond patterns).
  * - The incoming edges perform no side-effecting computations that would be
  *   observable from the other edges.
  * - This functionality does not have to operate in the IDE.
  *
  * Additionally, it will only trigger when the following conditions hold:
  *
  * - All dependent names are defined in the same method.
  * - The dependencies of the `@Auto_Parallel` computation may not be used
  *   except for inside the annotated call.
  * - The dependencies must be able to be inlined.
  */
object AutomaticParallelism extends IRPass {
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = Configuration
  override val precursorPasses: Seq[IRPass] = Seq(
    AliasAnalysis,
    DataflowAnalysis,
    ComplexType
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
  ): IR.Module = {
    val inlineContext = InlineContext.fromModuleContext(moduleContext)
    ir.copy(bindings =
      ir.bindings.map(processModuleDefinition(_, inlineContext))
    )
  }

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

  // If I can do the limited form, then it is sufficient to have spawn/await on
  //  bindings combined with liberal inlining of the other parts of the
  //  computation.
  //  - At the binding, spawn a thread and store an identifier into the cell.
  //  - At the read emit a special ReadVariableNode that joins on that thread.
  //  This becomes even easier if I can make assumptions about where the
  //  annotated expression is, but that isn't necessary for this approach.

  // TODO [AA] The steps are as follows:
  //   1. Walk over the IR, building a mapping from UUID to node along the way.
  //   2. Get the dependency sets and ensure that they are disjoint.
  //   3. Perform inlining.
  //   4. Annotate the block with the parallel streams (structure TBC)
  //   5. Emit a warning when the annotation cannot be obeyed.
  //   6. Docs and cleanup

  // === Pass Implementation ==================================================

  def processModuleDefinition(
    binding: Definition,
    @unused inlineContext: InlineContext
  ): Definition = {
    binding match {
      case method: Definition.Method.Explicit => method
      case atom: Definition.Atom              => atom
      case _: Definition.Type =>
        throw new CompilerError(
          "Complex type definitions should not be present at the point of " +
          "parallelism analysis."
        )
      case _: Definition.Method.Binding =>
        throw new CompilerError(
          "Binding-style methods should be desugared by the point of " +
          "parallelism analysis."
        )
      case _: Name.Annotation =>
        throw new CompilerError(
          "Annotations should be desugared by the point of parallelism " +
          "analysis."
        )
      case _: Type.Ascription =>
        throw new CompilerError(
          "Type ascriptions should be desugared by the point of parallelism " +
          "analysis."
        )
      case _: IR.Comment =>
        throw new CompilerError(
          "Type ascriptions should be desugared by the point of parallelism " +
          "analysis."
        )
      case err: Error => err
    }
  }

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
