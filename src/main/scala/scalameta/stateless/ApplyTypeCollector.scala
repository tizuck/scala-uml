package scalameta.stateless

import scalameta.util.context.CollectorContext
import scalameta.util.namespaces.{DefaultNamespace, Entry}

import scala.meta.{Stat, Type}


case class ApplyTypeCollector(namespace:Entry,target:String,oStat:Option[Stat])

object ApplyTypeCollector {
  def apply(tpe:Type)(implicit context:CollectorContext): ApplyTypeCollector = tpe match {
    case Type.Name(name) =>
      val lookup = context
        .globalCon
        .findSpec(
          name,
          None,
          context.localCon.currentCompilationUnit,
          context.localCon.currentNamespace,
          context.localCon.currentImports)

      new ApplyTypeCollector(
        lookup.map(_._1).getOrElse(context.localCon.currentNamespace),
        name,
        lookup.flatMap(_._2))

    case Type.Select(qual,name) =>
      val qualResolved = SelectRefCollector(qual)

      val lookup = context
        .globalCon
        .findSpec(
          name.value,
          Some(qualResolved.namespaceAddition),
          context.localCon.currentCompilationUnit,
          context.localCon.currentNamespace,
          context.localCon.currentImports
        )

      new ApplyTypeCollector(
        lookup.map(_._1).getOrElse(context.localCon.currentNamespace),
        name.value,
        lookup.flatMap(_._2)
      )
  }
}
