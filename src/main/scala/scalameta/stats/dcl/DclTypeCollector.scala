package scalameta.stats.dcl
import scalameta.CollectorContext
import scalameta.common.RelationshipBaseCollector
import uml.{Association, Class, FromTo, Inner, Operation, Relationship, RelationshipInfo, UMLElement, Without}

import scala.meta.Decl

case class DclTypeRelationshipCollector(override val definedElements: List[UMLElement]
                                        , override val resultingContext: CollectorContext
                                        )
  extends RelationshipBaseCollector

object DclTypeRelationshipCollector {
  def apply(dclType:Decl.Type)(implicit context : CollectorContext): DclTypeRelationshipCollector = {
    //@todo types can have generic parameters and a type may bound parameters to Lowerbound or to Bounds
    val typeClass = Class(true,dclType.name.value,Nil,Nil,Nil,None,Some("type"))
    //@todo problems if two classes have the same name for a type
    val newContext = context.copy(definedTemplates = typeClass :: context.definedTemplates)
    val relationshipInfo = RelationshipInfo(None,None,context.thisPointer.get,typeClass,None,Without)
    val relationship = Relationship(Inner,FromTo,relationshipInfo,None)
    new DclTypeRelationshipCollector(relationship :: typeClass :: Nil,newContext)
  }
}
