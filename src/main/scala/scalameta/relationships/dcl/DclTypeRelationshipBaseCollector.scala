package scalameta.relationships.dcl
import scalameta.CollectorContext
import uml.{Association, Class, FromTo, Relationship, RelationshipInfo, Without}

import scala.meta.Decl

case class DclTypeRelationshipBaseCollector(override val relationships: List[Relationship]
                                            , override val resultingContext: CollectorContext
                                            , override val typeClass: Option[Class])
  extends DclRelationshipBaseCollector

object DclTypeRelationshipBaseCollector {
  def apply(dclType:Decl.Type)(implicit context : CollectorContext): DclTypeRelationshipBaseCollector = {
    //@todo types can have generic parameters and a type may bound parameters to Lowerbound or to Bounds
    val typeClass = Class(true,dclType.name.value,Nil,Nil,Nil,None,Some("type"))
    //@todo problems if two classes have the same name for a type
    val newContext = context.copy(definedTemplates = typeClass :: context.definedTemplates)
    val relationshipInfo = RelationshipInfo(Some("1"),Some("1"),context.thisPointer.get,typeClass,None,Without)
    val relationship = Relationship(Association,FromTo,relationshipInfo,None)
    new DclTypeRelationshipBaseCollector(List(relationship),newContext,Some(typeClass))
  }
}
