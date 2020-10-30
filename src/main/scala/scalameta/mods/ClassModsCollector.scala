package scalameta.mods

import uml.Attribute

import scala.meta.{Mod, Term}

case class ClassModsCollector(modifier:List[Attribute], stereotype:Option[String], isAbstract:Boolean){

  def +(other:Attribute):ClassModsCollector = this.copy(modifier = other :: this.modifier)
  def +(other:String):ClassModsCollector = this.copy(stereotype = Some(other))
  def +(other:Boolean):ClassModsCollector = this.copy(isAbstract = other)

}

object ClassModsCollector {
  def modsAttributeManifest:Map[Mod,Attribute] = Map (
    Mod.Final -> Attribute(None,None,"isFinal",None,None),
    Mod.Protected -> Attribute(None,None,"isProtected",None,None),
    Mod.Implicit -> Attribute(None,None,"isImplicit",None,None),
    Mod.Inline -> Attribute(None,None,"isInline",None,None),
    Mod.Lazy -> Attribute(None,None,"isLazy",None,None),
    Mod.Opaque -> Attribute(None,None,"isOpaque",None,None),
    Mod.Open -> Attribute(None,None,"isOpen",None,None),
    Mod.Override -> Attribute(None,None,"isOverride",None,None),
    Mod.Private -> Attribute(None,None,"isPrivate",None,None)
  )

  def apply(mods:List[Mod]): ClassModsCollector = {
    mods.foldLeft(ClassModsCollector(Nil,None,false)){
      case (acc,Mod.Final()) => acc + Attribute(None,None,"isFinal",None,None)
      case (acc,Mod.Private(ref)) => acc + Attribute(None,None,"isPrivate",None,Some(s"<<private in=${ref.syntax}"))
      case (acc,Mod.Override()) => acc + Attribute(None,None,"isOverride",None,None)
      case (acc,Mod.Open()) => acc + Attribute(None,None,"isOpen",None,None)
    }
  }

  private def addAttribute(atts:List[Attribute]):List[Attribute] = {
    atts
  }
}


