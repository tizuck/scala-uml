package scalameta.stats.defn.toplevel

import scalameta.relationships.dcl.DclDefCollector
import scalameta.stats.dcl.DclCollector
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Class, Operation, UMLElement}

import scala.meta.{Decl, Defn, Type}

case class DefnDefToplevelCollector(override val definedElements: List[UMLElement],
                                    override val resultingContext: CollectorContext)
  extends BaseCollector

object DefnDefToplevelCollector {
  def apply(defnDef:Defn.Def)(implicit context:CollectorContext): DefnDefToplevelCollector = {
    val dclRels =
      DclCollector(
        Decl.Def(defnDef.mods,
          defnDef.name,
          defnDef.tparams,
          defnDef.paramss,
          defnDef.decltpe.getOrElse(Type.Name("#notype#"))))(
            if(defnDef.decltpe.isEmpty){context.copy(localCon = context.localCon.copy(typeRequired = false))}else{context}
      )
    val operation = dclRels.definedElements.flatMap{
      case o:Operation => Some(o)
      case _ => None
    }

    new DefnDefToplevelCollector(
      List(Class(
        false,
        defnDef.name.value.toUpperCase,
        Nil,
        operation,
        Nil,
        None,
        Some("def")
      )),
      context
    )
  }
}
