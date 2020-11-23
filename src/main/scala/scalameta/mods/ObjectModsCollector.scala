package scalameta.mods

import scalameta.util.context.CollectorContext
import uml.{Attribute, Compartment, Stereotype}

import scala.meta.Mod



case class ObjectModsCollector(modifiers:List[Compartment], objectStereotypes:List[Stereotype]) {

}

object ObjectModsCollector {
  def apply(mods:List[Mod])(implicit context:CollectorContext): ObjectModsCollector = {
    val classMods = ClassModsCollector(mods)
    val caseObjectStereotype =
      classMods
        .classStereotypes
        .map( s =>
        if(s.equals(Stereotype("caseclass",Nil))){Stereotype("caseobject",Nil)}
        else throw new IllegalArgumentException(s"unexpected stereotype: $s")
      )
    new ObjectModsCollector(classMods.mods,caseObjectStereotype)
  }
}
