package scalameta.relationships.dcl

import scalameta.{CollectorContext, StateChangingCollector}
import uml.{Class, Relationship}

import scala.meta.Decl

case class DclRelationshipCollector(relationships:List[Relationship],
                                    typeClass:Option[Class],
                                    override val resultingContext: CollectorContext) extends StateChangingCollector

object DclRelationshipCollector {
  def apply(dcl:Decl)(implicit context: CollectorContext): DclRelationshipCollector = {
    val dclRelationsships = dcl match {
      case d : Decl.Val => Some(DclValRelationshipCollector(d))
      case d : Decl.Var => Some(DclVarRelationshipCollector(d))
      case d : Decl.Type => Some(DclTypeRelationshipBaseCollector(d))
      case _ => None
    }

    val relationships = if(dclRelationsships.isDefined){ dclRelationsships.get.relationships } else Nil
    val newContext = if(dclRelationsships.isDefined){ dclRelationsships.get.resultingContext } else context
    val typeClass = if(dclRelationsships.isDefined){dclRelationsships.get.typeClass} else None

    new DclRelationshipCollector(relationships,typeClass,newContext)

  }
}
