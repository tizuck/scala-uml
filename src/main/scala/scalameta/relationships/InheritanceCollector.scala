package scalameta.relationships

import plantuml.{Extension, Relationship, RelationshipInfo, ToFrom, Without}
import scalameta.CollectorContext
import scalameta.common.TypeNameCollector

import scala.meta.Init

case class InheritanceCollector(inheritance:Relationship)

object InheritanceCollector {
  def apply(init:Init)(implicit context:CollectorContext): InheritanceCollector = {
    val extendedType = TypeNameCollector(init.tpe)
    val inheritance = Relationship(
      Extension,
      ToFrom,
      RelationshipInfo(None,None,extendedType.typeRep,context.thisPointer,None,Without)
    )(None)
    new InheritanceCollector(inheritance)
  }
}
