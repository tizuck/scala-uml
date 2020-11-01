package scalameta.stats.defn


import scalameta.stats.dcl.DclCollector
import scalameta.util.{BaseCollector, CollectorContext}
import uml.{ UMLElement}

import scala.meta.{Decl, Defn, Type}

case class DefnCollector(definedElements:List[UMLElement],
                         override val resultingContext: CollectorContext) extends BaseCollector

object DefnCollector {
  def apply(defn:Defn)(implicit context:CollectorContext): DefnCollector = {
    val defnRelationships = defn match {
      case Defn.Val(mods, pats, optionType , _) =>
        val dclRels = DclCollector(Decl.Val(mods,pats,optionType.getOrElse(Type.Name(""))))
        fromDecl(dclRels)
      case Defn.Var(mods,pats,optionType,_) =>
        val dclRels = DclCollector(Decl.Var(mods,pats,optionType.getOrElse(Type.Name(""))))
        fromDecl(dclRels)
      case Defn.Def(mods, name, tparams, paramss, maybeType, _) =>
        val dclRels = DclCollector(Decl.Def(mods,name,tparams,paramss , maybeType.getOrElse(Type.Name(""))))
        fromDecl(dclRels)
      case Defn.Type(mods,pats,typeparams,_) =>
        //@todo clearance upon what happens with the body of a type definition
      val dclRels = DclCollector(Decl.Type(mods,pats,typeparams,Type.Bounds(None,None)))
        fromDecl(dclRels)
      case t : Defn.Trait => DefnTraitCollector(t)
      case c : Defn.Class => DefnClassCollector(c)
      case o : Defn.Object => DefnObjectCollector(o)
      case e : Defn.Enum => DefnEnumCollector(e)
      case re : Defn.RepeatedEnumCase => DefnRepeatedEnumCaseCollector(re)
      case ec : Defn.EnumCase => DefnEnumCaseCollector(ec)
    }

    new DefnCollector(
      defnRelationships.definedElements,
      defnRelationships.resultingContext
    )
  }

  private def fromDecl(dclRelationshipCollector: DclCollector):BaseCollector = {
    new BaseCollector {
      override val definedElements: List[UMLElement] = dclRelationshipCollector.definedElements
      override val resultingContext: CollectorContext = dclRelationshipCollector.resultingContext
    }
  }
}
