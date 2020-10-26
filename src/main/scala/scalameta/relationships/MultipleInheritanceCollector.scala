package scalameta.relationships

import plantuml.Relationship
import scalameta.CollectorContext

import scala.meta.Defn

case class MultipleInheritanceCollector(inheritance:List[Relationship])

object MultipleInheritanceCollector {
  def apply(defn:Defn)(implicit context : CollectorContext): MultipleInheritanceCollector = defn match {
    case Defn.Trait(_,name,_,_,templ) =>
     val inheritances = for(init <- templ.inits) yield {
        InheritanceCollector(init)(context.copy(thisPointer = name.value)).inheritance
      }

      new MultipleInheritanceCollector(inheritances)
  }

}
