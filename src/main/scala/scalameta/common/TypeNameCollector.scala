package scalameta.common

import sun.reflect.generics.reflectiveObjects.NotImplementedException

import scala.meta.Type

case class TypeNameCollector(typeRep:String)

object TypeNameCollector {
  def apply(mtype:scala.meta.Type): TypeNameCollector = {
    mtype match {
      case Type.Var(name) => new TypeNameCollector(name.syntax)
      case Type.Name(name) => new TypeNameCollector(name)
      case _ => throw new NotImplementedException
    }
  }
}
