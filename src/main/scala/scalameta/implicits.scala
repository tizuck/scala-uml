package scalameta

import cats.kernel.Semigroup
import cats.implicits._
import scalameta.util.context.{CollectorContext, GlobalContext, LocalContext}
import scalameta.util.namespaces.DefaultNamespace
import uml.UMLUnit

object implicits {

  implicit val sourceCollectorSemiGroup:Semigroup[SourceCollector] = (x: SourceCollector, y: SourceCollector) => {
    val xToplevel = x.umlUnit.toplevelElements
    val yToplevel = y.umlUnit.toplevelElements

    val xResultingExternals = x.resultingContext.localCon.externalReferences
    val yResultingExternals = y.resultingContext.localCon.externalReferences

    SourceCollector(UMLUnit(x.umlUnit.identifier, xToplevel |+| yToplevel), x.resultingContext |+| y.resultingContext)
  }

  /**
   * @todo not associative but for the purpose it is not needed
   */
  implicit val collectorContextSemiGroup:Semigroup[CollectorContext] = (x: CollectorContext, y: CollectorContext) => {
    CollectorContext(
      LocalContext(
        None,
        "",
        None,
        x.localCon.currentImports |+| y.localCon.currentImports,
        DefaultNamespace,
        true,
        true,
        x.localCon.externalReferences |+| y.localCon.externalReferences,
        x.localCon.opReps
      ),
      GlobalContext(
        x.globalCon.globalScope |+| y.globalCon.globalScope
      )
    )
  }
}
