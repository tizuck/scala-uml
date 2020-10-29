package scalameta.stats

import scalameta.stats.dcl.DclCollector
import scalameta.stats.defn.DefnCollector
import scalameta.util.{BaseCollector, CollectorContext, StateChangingCollector}
import uml.UMLElement

import scala.meta.{Decl, Defn, Stat}

case class StatCollector(definedElements : List[UMLElement],
                         override val resultingContext: CollectorContext)
  extends StateChangingCollector

object StatCollector {
  def apply(stat:Stat)(implicit context: CollectorContext): StatCollector = {
    val relBase : BaseCollector = stat match {
      case decl: Decl => DclCollector(decl)
      case defn: Defn => DefnCollector(defn)
    }

    new StatCollector(definedElements = relBase.definedElements,resultingContext = relBase.resultingContext)
  }
}
