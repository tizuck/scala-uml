package scalameta.relationships

import plantuml.{Association, FromTo, Relationship, RelationshipInfo}
import scalameta.CollectorContext
import scalameta.common.TypeNameCollector
import sun.reflect.generics.reflectiveObjects.NotImplementedException

import scala.meta.{Decl, Pat, Type}

case class DclRelationshipCollector(relationships:List[Relationship])

object DclRelationshipCollector {
  def apply(dcl:Decl)(implicit context: CollectorContext): DclRelationshipCollector = {
    val relationships = dcl match {
      case Decl.Val(mods,pats,decltpe) =>
        val assocInfo = collectAssociationInformation(pats,decltpe)

        val relationships = assocInfo.pSources.map{
          s =>
            Relationship(
              Association,
              FromTo,
              RelationshipInfo(
                None,
                Some(assocInfo.targetMultiplicity),
                context.thisPointer,
                assocInfo.pDeclType,Some(s),
                FromTo))(None)
        }
        Some(new DclRelationshipCollector(relationships))
      case Decl.Var(mods,pats,decltpe) =>
        val assocInfo = collectAssociationInformation(pats,decltpe)

        val relationships = assocInfo.pSources.map{
          s =>
            Relationship(
              Association,
              FromTo,
              RelationshipInfo(
                None,
                Some(assocInfo.targetMultiplicity),
                context.thisPointer,
                assocInfo.pDeclType,Some(s),
                FromTo))(Some("var"))
        }
        Some(new DclRelationshipCollector(relationships))
      case _ => None
    }

    relationships.getOrElse(new DclRelationshipCollector(Nil))
  }

  trait AssociationInformation {
    val pDeclType : String
    val targetMultiplicity : String
    val pSources : List[String]
  }
  private def collectAssociationInformation(pats:List[Pat],decltpe:Type)(
    implicit context: CollectorContext
  ) : AssociationInformation = {
    new AssociationInformation {
      override val pDeclType: String = TypeNameCollector(decltpe).typeRep
      override val targetMultiplicity: String = TargetMultiplicityCollector(decltpe).multiplicity
      override val pSources: List[String] = pats.collect { _.syntax }
    }
  }
}
