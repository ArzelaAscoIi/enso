package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Definition
import org.enso.compiler.core.IR.{Application, Case, DefinitionArgument, Error, Name, Type}
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.DataflowAnalysis.DependencyInfo
import org.enso.compiler.pass.desugar.ComplexType
import org.enso.compiler.pass.resolve.ExpressionAnnotations

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
    processExpression(ir, new MutablePassData(inlineContext), ScopedPassData())
  }

  // If I can do the limited form, then it is sufficient to have spawn/await on
  //  bindings combined with liberal inlining of the other parts of the
  //  computation.
  //  - At the binding, spawn a thread and store an identifier into the cell.
  //  - At the read emit a special ReadVariableNode that joins on that thread.
  //  This becomes even easier if I can make assumptions about where the
  //  annotated expression is, but that isn't necessary for this approach.

  // === Pass Implementation ==================================================

  private def processModuleDefinition(
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

  private def processMethod(
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

  private def processExpression(
    expr: IR.Expression,
    mutData: MutablePassData,
    scopedData: ScopedPassData
  ): IR.Expression = {
    val result = expr match {
      case func: IR.Function   => processFunction(func, mutData, scopedData)
      case app: IR.Application => processApplication(app, mutData, scopedData)
      case cse: IR.Case        => processCase(cse, mutData, scopedData)
      case block: IR.Expression.Block =>
        try {
          val newScopedData = scopedData.addEnclosingBlock(block)
          val newExpressions =
            block.expressions.map(processExpression(_, mutData, newScopedData))
          val newRet =
            processExpression(block.returnValue, mutData, newScopedData)
          block.copy(expressions = newExpressions, returnValue = newRet)
        } catch {
          case _: RewriteException => ???
        }
      case binding: IR.Expression.Binding =>
        val newExpr =
          processExpression(binding.expression, mutData, scopedData)
        binding.copy(expression = newExpr)
      case name: IR.Name       => name
      case lit: IR.Literal     => lit
      case comm: IR.Comment    => comm
      case foreign: IR.Foreign => foreign
      case empty: IR.Empty     => empty
      case error: IR.Error     => error
      case typ: Type           => typ
    }
    mutData.putExpr(result)
    result
  }

  // TODO [AA] The steps are as follows:
  //   1. Walk over the IR, building a mapping from UUID to node along the way.
  //   2. Get the dependency sets and ensure that they are disjoint, and not
  //      touched by others, and do not depend on function args
  //   3. Perform inlining.
  //   4. Annotate the block with the parallel streams (structure TBC)
  //   5. Emit a warning when the annotation cannot be obeyed.
  //   6. Docs and cleanup
  //  How do I get the inlined blocks to the right place?

  private def processParallelCall(
    app: Application.Prefix,
    @unused mutData: MutablePassData,
    @unused scopedData: ScopedPassData
  ): IR.Expression = {
    println("========== Rewriting ==========")
    // TODO [AA] Check that the function does not depend on args
    val dataflow = app
      .getMetadata(DataflowAnalysis)
      .getOrElse(
        throw new CompilerError(
          "Dataflow analysis metadata is required for parallelism analysis."
        )
      )
    val args = app.arguments.collect { case a: IR.CallArgument.Specified => a }

    val firstArg = args.head
    val dep = mkStaticDep(firstArg)
    val flow = dataflow.get(dep)

    // TODO [AA] Add a way to get dependencies

    println(flow)
    println(app.getId)
    println("========== Done ==========")
    app
  }

  private def mkStaticDep(ir: IR): DependencyInfo.Type.Static = {
    DependencyInfo.Type.Static(ir.getId, ir.getExternalId)
  }

  private def processApplication(
    app: IR.Application,
    mutData: MutablePassData,
    scopedData: ScopedPassData
  ): IR.Expression = {
    val result: IR.Expression = app match {
      case app @ Application.Prefix(function, arguments, _, _, _, _) =>
        val isParAnnotated = app
          .getMetadata(ExpressionAnnotations)
          .exists(m =>
            m.annotations.exists(ann =>
              ann.name == ExpressionAnnotations.autoParallelName
            )
          )
        if (isParAnnotated) {
          processParallelCall(app, mutData, scopedData)
        } else {
          val newFn = processExpression(function, mutData, scopedData)
          val newArgs = arguments.map {
            case spec @ IR.CallArgument.Specified(_, value, _, _, _, _) =>
              val newExpr = processExpression(value, mutData, scopedData)
              val result  = spec.copy(value = newExpr)
              mutData.putCallArgument(result)
              result
          }
          app.copy(function = newFn, arguments = newArgs)
        }
      case force @ Application.Force(target, _, _, _) =>
        force.copy(target = processExpression(target, mutData, scopedData))
      case sequence: Application.Literal.Sequence =>
        val newItems =
          sequence.items.map(processExpression(_, mutData, scopedData))
        sequence.copy(items = newItems)
      case _: Application.Literal.Typeset =>
        throw new CompilerError(
          "Typeset literals not supported in parallelism analysis."
        )
      case _: Application.Operator =>
        throw new CompilerError(
          "Operators should be desugared to functions by the point of " +
          "parallelism analysis."
        )
    }
    mutData.putExpr(result)
    result
  }

  private def processFunction(
    function: IR.Function,
    passData: MutablePassData,
    scopedData: ScopedPassData
  ): IR.Expression = function match {
    case fn @ IR.Function.Lambda(arguments, body, _, _, _, _) => {
      val processedArguments = arguments.map {
        case arg @ DefinitionArgument.Specified(_, default, _, _, _, _) =>
          val newDefault =
            default.map(processExpression(_, passData, scopedData))
          val result = arg.copy(defaultValue = newDefault)
          passData.putDefinitionArg(result)
          result
      }
      val processedBody = processExpression(body, passData, scopedData)
      fn.copy(arguments = processedArguments, body = processedBody)
    }
    case _: IR.Function.Binding =>
      throw new CompilerError(
        "Binding-style functions should be desugared by the point of " +
        "parallelism analysis."
      )
  }

  private def processCase(
    cse: IR.Case,
    mutData: MutablePassData,
    scopedData: ScopedPassData
  ): IR.Expression = cse match {
    case expr @ Case.Expr(scrutinee, branches, _, _, _) =>
      val newScrut    = processExpression(scrutinee, mutData, scopedData)
      val newBranches = branches.map(processCaseBranch(_, mutData, scopedData))
      expr.copy(scrutinee = newScrut, branches = newBranches)
    case _: Case.Branch =>
      throw new CompilerError("Unexpected case branch in processCase")
  }

  private def processCaseBranch(
    branch: IR.Case.Branch,
    mutData: MutablePassData,
    scopedData: ScopedPassData
  ): IR.Case.Branch = {
    val result =
      branch.copy(expression =
        processExpression(branch.expression, mutData, scopedData)
      )
    mutData.putExpr(result)
    result
  }

  // === Internal Data ========================================================

  private case class RewriteException() extends Exception

  /** Pass data that is immutable for the pass.
    *
    * @param enclosingBlocks the block that encloses the current expression
    */
  private case class ScopedPassData(
    enclosingBlocks: Seq[IR.Expression.Block] = Seq()
  ) {

    /** Adds an enclosing block onto the stack.
      *
      * @param block the new enclosing block
      * @return a new instance of the pass data
      */
    def addEnclosingBlock(block: IR.Expression.Block): ScopedPassData = {
      this.copy(enclosingBlocks = enclosingBlocks :+ block)
    }
  }

  /** Mutable used to perform the analysis in this pass.
    *
    * @param context the compiler context for the pass
    */
  // TODO [AA] Make private
  class MutablePassData(val context: InlineContext) {
    private[this] val callArgs: mutable.Map[IR.Identifier, IR.CallArgument] =
      mutable.Map()
    private[this] val definitionArgs
      : mutable.Map[IR.Identifier, IR.DefinitionArgument] =
      mutable.Map()
    private[this] val expressions: mutable.Map[IR.Identifier, IR.Expression] =
      mutable.Map()

    /** Stores a call argument in the pass data.
      *
      * @param arg the argument to store
      */
    def putCallArgument(arg: IR.CallArgument): Unit = {
      callArgs += arg.getId -> arg
    }

    /** Gets a call argument from the pass data.
      *
      * @param id the identifier to get an argument for
      * @return the argument associated with `id`, if it exists
      */
    def getCallArg(id: IR.Identifier): Option[IR.CallArgument] = {
      callArgs.get(id)
    }

    /** Stores a definition argument in the pass data.
      *
      * @param arg the argument to store
      */
    def putDefinitionArg(arg: IR.DefinitionArgument): Unit = {
      definitionArgs += arg.getId -> arg
    }

    /** Gets a definition argument from the pass data.
      *
      * @param id the identifier to get an argument for
      * @return the argument associated with `id`, if it exists
      */
    def getDefinitionArg(id: IR.Identifier): Option[IR.DefinitionArgument] = {
      definitionArgs.get(id)
    }

    /** Store the IR in the pass data.
      *
      * @param ir the IR to store
      */
    def putExpr(ir: IR.Expression): Unit = {
      expressions += ir.getId -> ir
    }

    /** Get the IR by the specified `id`, if it exists.
      *
      * @param id the identifier to get the IR for
      * @return the IR associated with `id`, if it exists
      */
    def getExpr(id: IR.Identifier): Option[IR.Expression] = {
      expressions.get(id)
    }

    /** Unsafely gets the IR associated with `id`.
      *
      * This function will cause a crash if no IR exists for the provided `id`.
      *
      * @param id the identifier to get the IR for
      * @return the IR associated with `id`
      */
    def unsafeGetIr(id: IR.Identifier): IR = {
      this.getExpr(id).get
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
