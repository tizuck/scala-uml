package scalameta.stateless

import scalameta.util.context.CollectorContext
import uml.{Modificator, Static}

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