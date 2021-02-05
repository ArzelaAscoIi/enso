package org.enso.compiler.test.pass.analyse

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.PassConfiguration.ToPair
import org.enso.compiler.pass.analyse.{AliasAnalysis, AutomaticParallelism}
import org.enso.compiler.pass.optimise.ApplicationSaturation
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class AutomaticParallelismTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes(autoParallelismEnabled = true)

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

  "Automatic parallelism analysis" should {}

}
