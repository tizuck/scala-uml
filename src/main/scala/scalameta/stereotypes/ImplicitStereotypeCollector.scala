package scalameta.stereotypes

import scala.meta.Mod

case class ImplicitStereotypeCollector(isImplicitStereotype:Boolean)

object ImplicitStereotypeCollector {
  def apply(mods:List[Mod]): ImplicitStereotypeCollector = {
    val collected = mods.collect{
      case Mod.Implicit() => true
    }
    val isImplicit = collected.exists(identity)
    new ImplicitStereotypeCollector(isImplicit)
  }
}
