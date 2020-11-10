package scalameta.stats.imports

import scala.meta.Importee

case class ImporteeNameCollector(name:String)

object ImporteeNameCollector {
  def apply(name:Importee.Name): ImporteeNameCollector = {
    ImporteeNameCollector(name.name.value)
  }
}
