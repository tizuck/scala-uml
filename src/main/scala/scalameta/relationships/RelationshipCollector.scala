package scalameta.relationships



import scalameta.relationships.dcl.DclRelationshipCollector
import scalameta.relationships.defn.DefnRelationshipCollector
import scalameta.relationships.inheritance.MultipleInheritanceCollector
import scalameta.{CollectorContext, StateChangingCollector}
import uml.Relationship
import uml.Class

import scala.meta.{Decl, Defn}

case class RelationshipCollector(relationships:List[Relationship],
                                 typeClasses:List[Class],
                                 override val resultingContext: CollectorContext) extends StateChangingCollector

object RelationshipCollector {
  def apply(defn:Defn)(implicit context:CollectorContext): RelationshipCollector = defn match {
    case Defn.Trait(_, _, _ , _ , template) =>
      val inheritances = MultipleInheritanceCollector(defn)

      template.stats.foldLeft(RelationshipCollector(inheritances.inheritance,Nil,inheritances.resultingContext)){
        case (acc,st) => st match {
          case decl: Decl =>
            val declCollector = DclRelationshipCollector(decl)(acc.resultingContext)
            acc.copy(relationships = acc.relationships ++ declCollector.relationships,
              typeClasses = acc.typeClasses ++ declCollector.typeClass.map(t => List(t)).getOrElse(Nil),
              resultingContext = declCollector.resultingContext)
          case defn: Defn =>
            val defnCollector = DefnRelationshipCollector(defn)(acc.resultingContext)
            acc.copy(relationships = acc.relationships ++ defnCollector.relationships,
              typeClasses = acc.typeClasses ++ defnCollector.typeClass.map(t => List(t)).getOrElse(Nil),
              resultingContext = defnCollector.resultingContext)
        }
      }
  }
}
