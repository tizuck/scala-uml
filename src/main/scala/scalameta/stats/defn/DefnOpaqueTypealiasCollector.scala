package scalameta.stats.defn

import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Attribute, Relationship, TaggedValue, UMLElement}

import scala.meta.Defn

case class DefnOpaqueTypealiasCollector(override val definedElements: List[UMLElement],
                                        override val resultingContext: CollectorContext)
  extends BaseCollector

object DefnOpaqueTypealiasCollector {
  def apply(opType:Defn.OpaqueTypeAlias)(implicit context:CollectorContext):DefnOpaqueTypealiasCollector = {
    val defnType = DefnTypeCollector(Defn.Type(opType.mods,opType.name,opType.tparams,opType.body))
    val typeFromCollector: List[uml.Class] = defnType.definedElements.flatMap{
      case c:uml.Class => Some(c)
      case _ => None
    }
    
    val relationship: List[Relationship] = defnType.definedElements.flatMap{
      case r:Relationship => Some(r)
      case _ => None
    }
    //@todo find out what opType.bounds on opaque types are
    DefnOpaqueTypealiasCollector(
      typeFromCollector
        .map( c =>
          c.copy(
            additionalCompartements =
              List(uml.Compartment(Some("<<ScalaClass>>"),List(TaggedValue("isOpaque",None)),Nil))
          )) ++ relationship,
        context
    )
  }
}
