package scalameta.relationships

import sun.reflect.generics.reflectiveObjects.NotImplementedException

import scala.meta.Type
import scala.meta.Type._

case class TargetMultiplicityCollector(multiplicity:String)

object TargetMultiplicityCollector {
  def apply(target : Type): TargetMultiplicityCollector = {
    target match {
      case Name(name) => new TargetMultiplicityCollector("1")
      case _ => throw new NotImplementedException
    }
  }
}
