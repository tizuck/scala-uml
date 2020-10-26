package scalameta.common

import scalameta.CollectorContext
import sun.reflect.generics.reflectiveObjects.NotImplementedException

import scala.meta.Type

case class TypeNameCollector(typeRep:String)

object TypeNameCollector {
  def apply(mtype:scala.meta.Type)(implicit context : CollectorContext): TypeNameCollector = {
    mtype match {
      case Type.Var(name) => new TypeNameCollector(name.syntax)
      case Type.Name(name) => new TypeNameCollector(name)
      case Type.Placeholder(bounds) =>
        val optBounds = BoundsNameCollector(bounds).bounds
        if(optBounds.isDefined)
          new TypeNameCollector(optBounds.get)
        else {
          new TypeNameCollector("WildcardType")
        }
      case Type.Apply(tpe,args) =>
        val applyType = TypeNameCollector(tpe).typeRep
        val argTypes = for (arg <- args) yield {
          TypeNameCollector(arg).typeRep
        }
        new TypeNameCollector(s"$applyType<${argTypes.mkString(",")}>")
    }
  }
}
