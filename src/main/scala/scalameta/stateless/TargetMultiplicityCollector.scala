package scalameta.stateless

import sun.reflect.generics.reflectiveObjects.NotImplementedException

import scala.meta.Type
import scala.meta.Type._

case class TargetMultiplicityCollector(multiplicity:String)

object TargetMultiplicityCollector {
  def apply(target : Type): TargetMultiplicityCollector = {
    target match {
      case Name(name) => new TargetMultiplicityCollector("1")
      case Apply(tpe, args) => tpe match {
        case Name(name) => name match {
          case "Option" => TargetMultiplicityCollector("[0..1]")
          case "List" => TargetMultiplicityCollector("*")
          case _ => TargetMultiplicityCollector("1")
        }
      }
      case _ => TargetMultiplicityCollector("unknown")
    }
  }
}
