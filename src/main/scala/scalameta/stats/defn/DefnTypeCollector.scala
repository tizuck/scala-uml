package scalameta.stats.defn

import scalameta.stateless.TypeNameCollector
import scalameta.stats.dcl.DclTypeCollector
import scalameta.typeparams.TypeParamsCollector
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Attribute, Class, ConcreteClass, Inner, Relationship, RelationshipInfo, Stereotype, ToFrom, UMLElement, Without}

import scala.meta.Defn

case class DefnTypeCollector(override val definedElements: List[UMLElement],
                             override val resultingContext: CollectorContext)
  extends BaseCollector

object DefnTypeCollector {
  def apply(dType:Defn.Type)(implicit context:CollectorContext): DefnTypeCollector = {
    val generics = TypeParamsCollector(dType.tparams)

    val typeRep = TypeNameCollector(dType.body)
    val attr = Attribute(None,None,"_type_",Some(typeRep.typeRep),List(Stereotype("typeDef",Nil)))
    val typeClass = Class(
      true,
      dType.name.value,
      List(attr),
      Nil,
      Nil,
      Option.when(generics.typeParams.nonEmpty)(generics.typeParams),
      List(Stereotype("type",Nil)),
      context.localCon.currentNamespace)

    //If the type is defined within another entity
    if(context.localCon.thisPointer.isDefined){
      val relationshipInfo = RelationshipInfo(None,None,context.localCon.thisPointer.get,ConcreteClass(typeClass),None,Without)
      val relationship = Relationship(Inner,ToFrom,relationshipInfo,Nil)
      new DefnTypeCollector(relationship :: typeClass :: Nil,context)
    }//if type is defined on toplevel
    else {
      new DefnTypeCollector(typeClass :: Nil,context)
    }
  }
}
