package scalameta

import scala.meta.Defn

class DefnPlantUMLCollector(defn:Defn) {

}

object DefnPlantUMLCollector {
  def apply(defn: Defn): DefnPlantUMLCollector = defn match {
    case Defn.Enum(value, name, value1, primary, template) =>

  }
}
