package scalameta.mods

import uml.{Attribute, Stereotype}

import scala.meta.Mod



case class ObjectModsCollector(modifiers:List[Attribute],stereotype:List[Stereotype]) {

}

object ObjectModsCollector {
  def apply(mods:List[Mod]): ObjectModsCollector = {
    val classMods = ClassModsCollector(mods)
    val caseObjectStereotype =
      classMods
        .stereotype
        .map( s =>
        if(s.equals(Stereotype("caseclass",Nil))){Stereotype("caseobject",Nil)}
        else throw new IllegalArgumentException(s"unexpected stereotype: $s")
      )
    new ObjectModsCollector(classMods.modifier,caseObjectStereotype)
  }
}
