package scalameta.mods

import uml.{Attribute, Stereotype, TaggedValue}

import scala.meta.{Mod, Term}

case class ClassModsCollector(modifier:List[Attribute], stereotype:List[Stereotype], isAbstract:Boolean){

  def +(other:Attribute):ClassModsCollector = this.copy(modifier = other :: this.modifier)
  def +(other:Stereotype):ClassModsCollector = this.copy(stereotype = this.stereotype ++ List(other))
  def +(other:Boolean):ClassModsCollector = this.copy(isAbstract = other)

}

object ClassModsCollector {

  def apply(mods:List[Mod]): ClassModsCollector = {
    mods.foldLeft(ClassModsCollector(Nil,Nil,false)){
      case (acc,Mod.Final()) => acc + Attribute(None,None,"isFinal",None,Nil)
      case (acc,Mod.Private(ref)) =>
        acc + Attribute(
          None,
          None,
          "isPrivate",
          None,
          List(Stereotype("private",List(TaggedValue("in",s"${ref.syntax}"))))
        )
      case (acc,Mod.Override()) => acc + Attribute(None,None,"isOverride",None,Nil)
      case (acc,Mod.Open()) => acc + Attribute(None,None,"isOpen",None,Nil)
      case (acc,Mod.Abstract()) => acc + true
      case (acc,Mod.Case()) => acc + Stereotype("caseclass",Nil)
      case (acc,Mod.Protected(ref)) =>
        acc + Attribute(
          None,
          None,
          "isPrivate",
          None,
          List(Stereotype("protected",List(TaggedValue("in",s"${ref.syntax}"))))
        )
      case (acc,Mod.Opaque()) => acc + Attribute(None,None,"isOpaque",None,Nil)
      case (acc,Mod.Lazy()) => acc + Attribute(None,None,"isLazy",None,Nil)
      case (acc,Mod.Implicit()) => acc + Attribute(None,None,"isImplicit",None,Nil)
      case (acc,Mod.Inline()) => acc + Attribute(None,None,"isInline",None,Nil)
      case (acc,Mod.Sealed()) => acc + Attribute(None,None,"isSealed",None,Nil)
    }
  }
}


