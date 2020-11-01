package scalameta.mods

import scala.meta.Mod

case class ParameterModsCollector(stereotype : Option[String]){
  def +(other:String):ParameterModsCollector =
    ParameterModsCollector(
      stereotype
      .map(s => s + ", " + other)
      .orElse(Some(other))
    )
}

object ParameterModsCollector{
  def apply(mods:List[Mod]): ParameterModsCollector = {
    mods.foldLeft(ParameterModsCollector(None)){
      case (acc,Mod.Implicit()) =>
        acc + "implicit"
      case (acc,Mod.Using()) =>
        acc + "using"
      case (acc,_) => acc
    }
  }
}
