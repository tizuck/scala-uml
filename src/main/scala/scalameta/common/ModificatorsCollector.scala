package scalameta.common

import plantuml.{Modificator, Static}
import scalameta.CollectorContext

import scala.meta.Mod

case class ModificatorsCollector(modificators:List[Modificator])

object ModificatorsCollector {
  def apply(mods:List[Mod])(implicit context : CollectorContext): ModificatorsCollector = {
    val modificators = for (mod <- mods) yield {
      mod match {
        case Mod.Final() => List(Static)
        case Mod.Abstract() => List(Static)
        case _ => Nil
      }
    }

    new ModificatorsCollector(modificators.flatten)
  }
}