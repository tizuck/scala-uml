package scalameta.typeparams

import scalameta.stateless.TypeNameCollector
import scalameta.util.context.CollectorContext
import uml.{GenericParameter, Stereotype}

import scala.meta.Type

case class TypeParamCollector(typeParam:GenericParameter,contextBounds:List[String])

object TypeParamCollector{
  def apply(typeParam:Type.Param)(implicit context:CollectorContext): TypeParamCollector = {
    val tParamMods = TypeParamModCollector(typeParam.mods)
    val parameterName = typeParam.name.value
    val stereotype = {
      if (tParamMods.isContravariant) {
        List(Stereotype("-",Nil))
      } else if(tParamMods.isCovariant) {
        List(Stereotype("+",Nil))
      } else Nil
    }
    val cbounds = TypeParamContextBoundsCollector(typeParam.cbounds).contextBounds

    val metaBound:Type.Bounds = typeParam.tbounds
    val bounds = TypeBoundsCollector(metaBound).typeRep
    val tparams = TypeParamsCollector(typeParam.tparams).typeParams
    val tparamsStringRep = if(tparams.nonEmpty){ s"<${tparams.map(_.pretty).mkString(",")}>"} else ""

    new TypeParamCollector(GenericParameter(parameterName + tparamsStringRep,bounds,stereotype),cbounds)
  }
}