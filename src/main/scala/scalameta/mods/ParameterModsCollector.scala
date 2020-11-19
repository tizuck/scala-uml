package scalameta.mods

import uml.Stereotype

import scala.meta.Mod

case class ParameterModsCollector(stereotype : List[Stereotype],fixme:Boolean = true)

object ParameterModsCollector{
  def apply(mods:List[Mod]): ParameterModsCollector = {
    mods.foldLeft(new ParameterModsCollector(Nil)){
      case (acc,Mod.Implicit()) =>
        acc.copy(acc.stereotype ++ List(Stereotype("implicit",Nil)))
      case (acc,Mod.Using()) =>
        acc.copy(acc.stereotype ++ List(Stereotype("using",Nil)))
      case (acc,_) => acc
    }
  }
}
