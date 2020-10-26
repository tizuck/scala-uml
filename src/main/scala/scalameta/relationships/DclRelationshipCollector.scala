package scalameta.relationships

import plantuml.{Association, FromTo, Relationship, RelationshipInfo}
import scalameta.CollectorContext
import scalameta.common.TypeNameCollector
import sun.reflect.generics.reflectiveObjects.NotImplementedException

import scala.meta.{Decl, Type}

case class DclRelationshipCollector(relationships:List[Relationship])

object DclRelationshipCollector {
  def apply(dcl:Decl)(implicit context: CollectorContext): DclRelationshipCollector = {
    val relationships = dcl match {
      case Decl.Val(mods,pats,decltpe) =>
        val pSources : List[String] = pats.collect { _.syntax }

        val pDeclType : String = TypeNameCollector(decltpe).typeRep

        val targetMultiplicity = TargetMultiplicityCollector(decltpe).multiplicity

        val relationships = pSources.map{
          //@TODO might not allways be no stereotype
          s => Relationship(Association,FromTo,RelationshipInfo(None,Some(targetMultiplicity),context.thisPointer,pDeclType,Some(s),FromTo))(None)
        }
        Some(new DclRelationshipCollector(relationships))
      case _ => None
    }

    relationships.getOrElse(new DclRelationshipCollector(Nil))
  }
}
