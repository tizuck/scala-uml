package scalameta.stats.dcl
import scalameta.stateless.TypeNameCollector
import scalameta.typeparams.TypeParamsCollector
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Association, Class, ClassRef, ConcreteClass, FromTo, Inner, Operation, Relationship, RelationshipInfo, Stereotype, ToFrom, UMLElement, Without}

import scala.meta.Decl

case class DclTypeCollector(override val definedElements: List[UMLElement]
                            , override val resultingContext: CollectorContext
                                        )
  extends BaseCollector

object DclTypeCollector {
  def apply(dclType:Decl.Type)(implicit context : CollectorContext): DclTypeCollector = {
    //@todo types can have generic parameters
    val generics = TypeParamsCollector(dclType.tparams)

    val typeClass = Class(
      true,
      dclType.name.value,
      Nil,
      Nil,
      Nil,
      Option.when(generics.typeParams.nonEmpty)(generics.typeParams),
      List(Stereotype("type",Nil)),
      context.localCon.currentNamespace)

    //If the type is defined within another entity
    if(context.localCon.thisPointer.isDefined){
      val relationshipInfo = RelationshipInfo(None,None,context.localCon.thisPointer.get,ConcreteClass(typeClass),None,Without)
      val relationship = Relationship(Inner,ToFrom,relationshipInfo,Nil)
      new DclTypeCollector(relationship :: typeClass :: Nil,context)
    }//if type is defined on toplevel
    else {
      new DclTypeCollector(typeClass :: Nil,context)
    }
  }
}
