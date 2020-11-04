package scalameta.util

import scalameta.util.context.CollectorContext

trait StateChangingCollector {
  val resultingContext : CollectorContext


}
