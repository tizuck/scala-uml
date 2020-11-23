package scalameta.typeparams

import scalameta.util.context.CollectorContext
import uml.GenericParameter

import scala.meta.Type

case class TypeParamsCollector(typeParams:List[GenericParameter],contextBounds:Map[String,List[String]])

object TypeParamsCollector {
  def apply(tparams: List[Type.Param])(implicit context:CollectorContext): TypeParamsCollector = {
    tparams.foldLeft(new TypeParamsCollector(Nil,Map.empty)){
      case (acc,tparam) =>
        val typeParamCol = TypeParamCollector(tparam)
        val generic = typeParamCol.typeParam
        val cBounds = typeParamCol.contextBounds

        acc.copy(acc.typeParams ++ List(generic),contextBounds = acc.contextBounds + (generic.identifier -> cBounds ))
    }
  }
}