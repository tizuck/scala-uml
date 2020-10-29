package scalameta.stats.defn

import scalameta.common.RelationshipBaseCollector
import scalameta.stats.dcl.DclRelationshipCollector
import scalameta.{CollectorContext, StateChangingCollector}
import uml.{Class, Relationship, UMLElement}

import scala.meta.{Decl, Defn, Type}

case class DefnRelationshipCollector(definedElements:List[UMLElement],
                                     override val resultingContext: CollectorContext) extends RelationshipBaseCollector

object DefnRelationshipCollector {
  def apply(defn:Defn)(implicit context:CollectorContext): DefnRelationshipCollector = {
    val defnRelationships = defn match {
      case Defn.Val(mods, pats, optionType , _) =>
        val dclRels = DclRelationshipCollector(Decl.Val(mods,pats,optionType.getOrElse(Type.Name(""))))
        fromDecl(dclRels)
      case Defn.Var(mods,pats,optionType,_) =>
        val dclRels = DclRelationshipCollector(Decl.Var(mods,pats,optionType.getOrElse(Type.Name(""))))
        fromDecl(dclRels)
      case Defn.Type(mods,pats,typeparams,_) =>
        //@todo clearance upon what happens with the body of a type definition
      val dclRels = DclRelationshipCollector(Decl.Type(mods,pats,typeparams,Type.Bounds(None,None)))
        fromDecl(dclRels)
      case t : Defn.Trait => DefnTraitRelationshipCollector(t)
    }

    new DefnRelationshipCollector(
      defnRelationships.definedElements,
      defnRelationships.resultingContext)
  }

  private def fromDecl(dclRelationshipCollector: DclRelationshipCollector):RelationshipBaseCollector = {
    new RelationshipBaseCollector {
      override val definedElements: List[UMLElement] = dclRelationshipCollector.definedElements
      override val resultingContext: CollectorContext = dclRelationshipCollector.resultingContext
    }
  }
}
