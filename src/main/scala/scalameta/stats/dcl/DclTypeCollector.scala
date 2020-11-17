package scalameta.stats.dcl
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Association, Class, ClassRef, ConcreteClass, FromTo, Inner, Operation, Relationship, RelationshipInfo, ToFrom, UMLElement, Without}

import scala.meta.Decl

case class DclTypeCollector(override val definedElements: List[UMLElement]
                            , override val resultingContext: CollectorContext
                                        )
  extends BaseCollector

object DclTypeCollector {
  def apply(dclType:Decl.Type)(implicit context : CollectorContext): DclTypeCollector = {
    //@todo types can have generic parameters and a type may bound parameters to Lowerbound or to Bounds
    val typeClass = Class(true,dclType.name.value,Nil,Nil,Nil,None,Some("type"),context.localCon.currentNamespace)
    //@todo problems if two classes have the same name for a type
    val relationshipInfo = RelationshipInfo(None,None,context.localCon.thisPointer.get,ConcreteClass(typeClass),None,Without)
    val relationship = Relationship(Inner,ToFrom,relationshipInfo,None)
    new DclTypeCollector(relationship :: typeClass :: Nil,context)
  }
}
