package scalameta.util.partial

import scala.meta.{Defn, Tree}

object partial {
  def templates(target:String):PartialFunction[Tree,List[String]] = {
    case Defn.Trait(_,name,tparams,_,_) if name.value.equals(target) =>
      tparams.map(_.name.value)
    case Defn.Class(_,name,tparams,_,_) if name.value.equals(target) =>
      tparams.map(_.name.value)
    case Defn.Enum(_,name,tparams,_,_) if name.value.equals(target) =>
      tparams.map(_.name.value)
  }
}
