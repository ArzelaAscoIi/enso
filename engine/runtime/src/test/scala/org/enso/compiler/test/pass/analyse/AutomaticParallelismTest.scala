package org.enso.compiler.test.pass.analyse

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Definition
import org.enso.compiler.data.CompilerConfig
import org.enso.compiler.pass.PassConfiguration.ToPair
import org.enso.compiler.pass.analyse.{AliasAnalysis, AutomaticParallelism}
import org.enso.compiler.pass.optimise.ApplicationSaturation
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class AutomaticParallelismTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes(CompilerConfig(autoParallelismEnabled = true))

  val precursorPasses: PassGroup =
    passes.getPrecursors(AutomaticParallelism).get

  val passConfig: PassConfiguration = PassConfiguration(
    AliasAnalysis         -->> AliasAnalysis.Configuration(),
    ApplicationSaturation -->> ApplicationSaturation.Configuration()
  )

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfig)

  implicit class AnalyseModule(ir: IR.Module) {
    def analyse(implicit ctx: ModuleContext): IR.Module =
      AutomaticParallelism.runModule(ir, ctx)
  }

  def mkModuleContext: ModuleContext = {
    buildModuleContext(freshNameSupply = Some(new FreshNameSupply))
  }

  // === The Tests ============================================================

  "Successful parallelism analysis" should {
    implicit val moduleContext: ModuleContext = mkModuleContext

    val code =
      """
        |fn f g =
        |    x = File.read "foo"
        |    y = File.read "bar"
        |    a = x.length
        |    b = y.length
        |
        |    @Auto_Parallel a.n b
        |""".stripMargin.preprocessModule.analyse

    val methodBody =
      code.bindings.head.asInstanceOf[Definition.Method.Explicit].body
    println(methodBody.showCode())

    "determine the separated flows" in {
      pending
    }

    "inline the flows" in {
      pending
    }

    "associate correct metadata with the block" in {
      pending
    }
  }

  "Failed parallelism analysis" should {
    "raise a warning when an intermediary is used outside the streams" in {
      pending
    }

    "raise a warning when dependencies cannot be inlined" in {
      pending
    }

    "raise a warning when dependencies are used outside the @Auto_Parallel call" in {
      pending
    }

    "raise a warning when the parallel call depends on input arguments" in {
      pending
    }
  }
}
