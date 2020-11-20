package scalameta.stats.defn


import scalameta.stats.dcl.DclCollector
import scalameta.stats.defn.toplevel.{DefnDefToplevelCollector, DefnValToplevelCollector}
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.UMLElement

import scala.meta.{Decl, Defn, Type}

case class DefnCollector(definedElements:List[UMLElement],
                         override val resultingContext: CollectorContext) extends BaseCollector

object DefnCollector {
  def apply(defn:Defn)(implicit context:CollectorContext): DefnCollector = {
    val defnRelationships = defn match {
      case v@Defn.Val(mods, pats, optionType , _) =>
        val dclRels = if(context.localCon.isTopLevel) {
          DefnValToplevelCollector(v)
        } else {
          DclCollector(
            Decl.Val(
              mods,pats,optionType.getOrElse(Type.Name("#notype#"))))(
            if(optionType.isEmpty){context.typeRequired} else context
          )
        }
        val ret = new BaseCollector  {
          override val definedElements: List[UMLElement] = dclRels.definedElements
          override val resultingContext: CollectorContext = context
        }
        fromDecl(ret)
      case Defn.Var(mods,pats,optionType,_) =>
        val dclRels =
          DclCollector(Decl.Var(mods,pats,optionType.getOrElse(Type.Name("#notype#"))))(
            if(optionType.isEmpty){context.typeRequired} else context
        )
        fromDecl(dclRels.copy(resultingContext = context))
      case d@Defn.Def(mods, name, tparams, paramss, optionType, _) =>
        val dclRels = if(context.localCon.isTopLevel){
          DefnDefToplevelCollector(d)(
            if(optionType.isEmpty){context.notTypeRequired} else context.typeRequired
          )
        } else {
          DclCollector(Decl.Def(mods,name,tparams,paramss , optionType.getOrElse(Type.Name("#notype#"))))(
            if(optionType.isEmpty){context.notTypeRequired} else context.typeRequired
          )
        }
        val ret = new BaseCollector  {
          override val definedElements: List[UMLElement] = dclRels.definedElements
          override val resultingContext: CollectorContext = context
        }
        fromDecl(ret)
      case t:Defn.Type => DefnTypeCollector(t)
      case t:Defn.OpaqueTypeAlias => DefnOpaqueTypealiasCollector(t)
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

  private def fromDecl(dclRelationshipCollector: BaseCollector):BaseCollector = {
    new BaseCollector {
      override val definedElements: List[UMLElement] = dclRelationshipCollector.definedElements
      override val resultingContext: CollectorContext = dclRelationshipCollector.resultingContext
    }
  }
}
