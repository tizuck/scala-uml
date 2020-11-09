package scalameta.stateless

import scala.meta.Type
import scala.meta.Type.{Apply, Name, Var}

case class TypeNameAssociationCollector(typeRep:String)

object TypeNameAssociationCollector {
  def apply(mType:Type): TypeNameAssociationCollector =  mType match {
    case Var(name) => TypeNameAssociationCollector(name.value)
    case Name(value) => TypeNameAssociationCollector(value)
    case Apply(name, args) =>
      new TypeNameAssociationCollector(TypeNameAssociationCollector(name).typeRep)
  }
}
