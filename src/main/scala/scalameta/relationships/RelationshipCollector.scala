package scalameta.relationships



import scalameta.{StateChangingCollector, CollectorContext}
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

      val foldRels = template.stats.foldLeft[(List[Relationship],CollectorContext)]((List.empty,inheritances.resultingContext)){
        case (acc,st) => st match {
          case decl:Decl =>
            val relationshipCollector = DclRelationshipCollector(decl)(acc._2)
            (acc._1 ++ relationshipCollector.relationships,acc._2 + relationshipCollector.resultingContext)
          case _ => acc
        }
      }

      val relationships = foldRels._1
      val afterRelsContext = foldRels._2

      val foldTypeClasses = template.stats.foldLeft[(List[Class],CollectorContext)]((List.empty,afterRelsContext)){
        case (acc,st) => st match {
          case declType: Decl.Type =>
            val typeClassCollector = DclRelationshipCollector(declType)(acc._2)
            (acc._1 ++ List(typeClassCollector.typeClass.get),acc._2 + typeClassCollector.resultingContext)
          case _ => acc
        }
      }
      new RelationshipCollector(inheritances.inheritance ++ relationships,foldTypeClasses._1,foldTypeClasses._2)
  }
}
