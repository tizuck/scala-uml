package scalameta.mods

import uml.Attribute

import scala.meta.{Mod, Term}

case class ClassModsCollector(modifier:List[Attribute], stereotype:Option[String], isAbstract:Boolean){

  def +(other:Attribute):ClassModsCollector = this.copy(modifier = other :: this.modifier)
  def +(other:String):ClassModsCollector = this.copy(stereotype = Some(other))
  def +(other:Boolean):ClassModsCollector = this.copy(isAbstract = other)

}

object ClassModsCollector {

  def apply(mods:List[Mod]): ClassModsCollector = {
    mods.foldLeft(ClassModsCollector(Nil,None,false)){
      case (acc,Mod.Final()) => acc + Attribute(None,None,"isFinal",None,None)
      case (acc,Mod.Private(ref)) => acc + Attribute(None,None,"isPrivate",None,Some(s"<<private in=${ref.syntax}"))
      case (acc,Mod.Override()) => acc + Attribute(None,None,"isOverride",None,None)
      case (acc,Mod.Open()) => acc + Attribute(None,None,"isOpen",None,None)
      case (acc,Mod.Abstract()) => acc + true
      case (acc,Mod.Case()) => acc + "caseclass"
      case (acc,Mod.Protected(ref)) => acc + Attribute(None,None,"isPrivate",None,Some(s"<<protected in=${ref.syntax}"))
      case (acc,Mod.Opaque()) => acc + Attribute(None,None,"isOpaque",None,None)
      case (acc,Mod.Lazy()) => acc + Attribute(None,None,"isLazy",None,None)
      case (acc,Mod.Implicit()) => acc + Attribute(None,None,"isImplicit",None,None)
      case (acc,Mod.Inline()) => acc + Attribute(None,None,"isInline",None,None)
      case (acc,Mod.Sealed()) => acc + Attribute(None,None,"isSealed",None,None)
    }
  }
}


