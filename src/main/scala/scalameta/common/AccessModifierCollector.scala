package scalameta.common

import uml.{AccessModifier, Private, Protected}

import scala.meta.Mod

case class AccessModifierCollector(accessModifier : Option[AccessModifier])

object AccessModifierCollector {
  def apply(mods:List[Mod]): AccessModifierCollector = {
    val accessMods = for(mod <- mods) yield {
      mod match {
        case Mod.Private(within) =>
          //@todo what to do if package private ?
          List(Private)
        case Mod.Protected(within) =>
          //@todo what to do if package protected ?
          List(Protected)
        case _ => Nil
      }
    }

    //Multiple Access modifier should not be allowed
    new AccessModifierCollector(accessMods.flatten.foldLeft[Option[AccessModifier]](None){
      case (Some(m),amod) => Some(m)
      case (None,amod) => Some(amod)
      case _ => None
    })

  }
}
