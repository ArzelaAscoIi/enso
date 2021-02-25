package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Definition
import org.enso.compiler.core.IR.{DefinitionArgument, Error, Name, Type}
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.desugar.ComplexType

import scala.annotation.unused
import scala.collection.mutable

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
  ): IR.Expression = {
    processExpression(ir, inlineContext, new PassData)
  }

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
    inlineContext: InlineContext
  ): Definition = {
    binding match {
      case method: Definition.Method.Explicit =>
        processMethod(method, inlineContext)
      case atom: Definition.Atom => atom
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

  def processMethod(
    method: Definition.Method.Explicit,
    context: InlineContext
  ): Definition.Method.Explicit = {
    val body = method.body match {
      case lam: IR.Function.Lambda => runExpression(lam, context)
      case _ =>
        throw new CompilerError("Explicit methods should only contain lambdas.")
    }
    method.copy(body = body)
  }

  def processExpression(
    expr: IR.Expression,
    context: InlineContext,
    passData: PassData
  ): IR.Expression = {
    passData.putIr(expr)
    expr match {
      case func: IR.Function   => processFunction(func, context, passData)
      case app: IR.Application => processApplication(app, context, passData)
      case name: IR.Name       => processName(name, context, passData)
      case cse: IR.Case        => processCase(cse, context, passData)
      case lit: IR.Literal     => processLiteral(lit, context, passData)
      case block: IR.Expression.Block => {
        block // TODO [AA]
      }
      case binding: IR.Expression.Binding => {
        binding // TODO [AA]
      }
      case comm: IR.Comment    => comm
      case foreign: IR.Foreign => foreign
      case empty: IR.Empty     => empty
      case error: IR.Error     => error
      case typ: Type           => typ
    }
  }

  def processFunction(
    function: IR.Function,
    context: InlineContext,
    passData: PassData
  ): IR.Expression = function match {
    case fn @ IR.Function.Lambda(arguments, body, _, _, _, _) => {
      val processedArguments = arguments.map {
        case arg @ DefinitionArgument.Specified(_, _, _, _, _, _) => arg
      }
      processedArguments.foreach(passData.putIr)
      val processedBody = processExpression(body, context, passData)
      fn.copy(arguments = processedArguments, body = processedBody)
    }
    case _: IR.Function.Binding =>
      throw new CompilerError(
        "Binding-style functions should be desugared by the point of " +
        "parallelism analysis."
      )
  }

  def processApplication(
    app: IR.Application,
    @unused context: InlineContext,
    @unused passData: PassData
  ): IR.Expression = app

  def processName(
    name: IR.Name,
    @unused context: InlineContext,
    @unused passData: PassData
  ): IR.Expression = name

  def processCase(
    cse: IR.Case,
    @unused context: InlineContext,
    @unused passData: PassData
  ): IR.Expression = cse

  def processLiteral(
    lit: IR.Literal,
    @unused context: InlineContext,
    @unused passData: PassData
  ): IR.Expression = lit

  // === Internal Data ========================================================

  /** Data used to perform the analysis in this pass.
    */
  class PassData {
    private[this] val mapping: mutable.Map[IR.Identifier, IR] =
      mutable.Map()

    /** Store the IR in the pass data.
      *
      * @param ir the IR to store
      */
    def putIr(ir: IR): Unit = {
      mapping += ir.getId -> ir
    }

    /** Get the IR by the specified `id`, if it exists.
      *
      * @param id the identifier to get the IR for
      * @return the IR associated with `id`, if it exists
      */
    def getIr(id: IR.Identifier): Option[IR] = {
      mapping.get(id)
    }

    /** Unsafely gets the IR associated with `id`.
      *
      * This function will cause a crash if no IR exists for the provided `id`.
      *
      * @param id the identifier to get the IR for
      * @return the IR associated with `id`
      */
    def unsafeGetIr(id: IR.Identifier): IR = {
      this.getIr(id).get
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
