package scalameta.mods

import uml.Attribute

import scala.meta.Mod



case class ObjectModsCollector(modifiers:List[Attribute],stereotype:Option[String]) {

}

object ObjectModsCollector {
  def apply(mods:List[Mod]): ObjectModsCollector = {
    val classMods = ClassModsCollector(mods)
    val caseObjectStereotype =
      classMods
        .stereotype
        .map( s =>
        if(s.equals("caseclass")){"caseobject"}
        else throw new IllegalArgumentException(s"unexpected stereotype: $s")
      )
    new ObjectModsCollector(classMods.modifier,caseObjectStereotype)
  }
}
