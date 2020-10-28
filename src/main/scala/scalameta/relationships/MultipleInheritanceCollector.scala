package scalameta.relationships

import scalameta.{CollectorContext, StateChangingCollector}
import uml.Relationship

import scala.meta.Defn

case class MultipleInheritanceCollector(inheritance:List[Relationship],
                                        override val resultingContext: CollectorContext) extends StateChangingCollector

object MultipleInheritanceCollector {
  def apply(defn:Defn)(implicit context : CollectorContext): MultipleInheritanceCollector = defn match {
    case Defn.Trait(_,name,_,_,templ) =>
     val inheritances = for(init <- templ.inits) yield {
        InheritanceCollector(init)
      }

      new MultipleInheritanceCollector(
        inheritances.map(_.inheritance),
        context ++ inheritances.map(_.resultingContext))
  }

}
