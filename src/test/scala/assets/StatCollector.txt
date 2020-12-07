package scalameta.stats

import scalameta.stats.dcl.DclCollector
import scalameta.stats.defn.DefnCollector
import scalameta.stats.imports.ImportCollector
import scalameta.util.context.CollectorContext
import scalameta.util.{BaseCollector, StateChangingCollector}
import uml.UMLElement

import scala.meta.{Decl, Defn, Import, Pkg, Stat}

case class StatCollector(definedElements : List[UMLElement],
                         override val resultingContext: CollectorContext)
  extends StateChangingCollector

object StatCollector {
  def apply(stat:Stat)(implicit context: CollectorContext): StatCollector = {
    val relBase : BaseCollector = stat match {
      case pkg:Pkg => PkgCollector(pkg)
      case imprt:Import => ImportCollector(imprt)
      case decl: Decl => DclCollector(decl)
      case defn: Defn => DefnCollector(defn)
      case _ => new BaseCollector {
        override val definedElements: List[UMLElement] = Nil
        override val resultingContext: CollectorContext = context
      }
    }

    new StatCollector(definedElements = relBase.definedElements,resultingContext = relBase.resultingContext)
  }
}
