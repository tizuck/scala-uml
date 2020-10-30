package scalameta.stats.defn

import scalameta.mods.ClassModsCollector
import scalameta.util.{BaseCollector, CollectorContext}
import uml.UMLElement

import scala.meta.Defn

class DefnClassCollector(override val definedElements: List[UMLElement],
                         override val resultingContext: CollectorContext)
  extends BaseCollector

object DefnClassCollector {
  def apply(defnClass:Defn.Class)(implicit context : CollectorContext): DefnClassCollector = {
    val mods = ClassModsCollector(defnClass.mods)
    
  }


}

