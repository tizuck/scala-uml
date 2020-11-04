package scalameta.typeparams

import scala.meta.Mod
import scala.meta.Mod.{Contravariant, Covariant}

case class TypeParamModCollector(isCovariant:Boolean=false,isContravariant:Boolean=false)

object TypeParamModCollector {
  def apply(mods:List[Mod]): TypeParamModCollector = {
    mods.foldLeft(TypeParamModCollector()){
      case (acc,mod) => mod match {
        case Contravariant() => acc.copy(isContravariant = true)
        case Covariant() => acc.copy(isCovariant = true)
      }
    }
  }
}
