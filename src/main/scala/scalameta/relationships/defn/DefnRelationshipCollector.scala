package scalameta.relationships.defn

import scalameta.relationships.dcl.DclRelationshipCollector
import scalameta.{CollectorContext, StateChangingCollector}
import uml.{Class, Relationship}

import scala.meta.{Decl, Defn, Type}

case class DefnRelationshipCollector(relationships:List[Relationship],
                                     typeClass:Option[Class],
                                      override val resultingContext: CollectorContext) extends StateChangingCollector

object DefnRelationshipCollector {
  def apply(defn:Defn)(implicit context:CollectorContext): DefnRelationshipCollector = {
    defn match {
      case Defn.Val(mods, pats, optionType , _) =>
        val dclRels = DclRelationshipCollector(Decl.Val(mods,pats,optionType.getOrElse(Type.Name(""))))
        DefnRelationshipCollector(dclRels)
    }
  }

  def apply(dclRels:DclRelationshipCollector): DefnRelationshipCollector ={
    new DefnRelationshipCollector(dclRels.relationships,
      dclRels.typeClass,
      dclRels.resultingContext)
  }
}
