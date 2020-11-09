package scalameta.util.namespaces.collector
import scalameta.util.namespaces.Entry

import scala.meta.{Defn, Pkg, Stat}

case class StatCollector(override val resultingMap: Map[Entry, List[Stat]])
  extends BaseNamespaceCollector

object StatCollector {
  def apply(stat:Stat): StatCollector = stat match {
    case pkg:Pkg => StatCollector(PkgCollector(pkg).resultingMap)
    case cls:Defn.Class =>StatCollector(ClassCollector(cls).resultingMap)
    case enum:Defn.Enum => StatCollector(EnumCollector(enum).resultingMap)
    case obj:Defn.Object => StatCollector(ObjectCollector(obj).resultingMap)
    case trt:Defn.Trait => StatCollector(TraitCollector(trt).resultingMap)
    case tpe:Defn.Type => StatCollector(TypeCollector(tpe).resultingMap)
    case _ => StatCollector(Map.empty[Entry,List[Stat]])
   }
}
