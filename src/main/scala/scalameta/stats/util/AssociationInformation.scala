package scalameta.stats.util

import scalameta.stateless.{TargetMultiplicityCollector, TargetTypeCollector, TypeNameAssociationCollector, TypeNameCollector}
import scalameta.util.context.CollectorContext

import scala.meta.{Pat, Type}

trait AssociationInformation {
  val pDeclType : TargetTypeCollector
  val targetMultiplicity : String
  val pSources : List[String]
}

object AssociationInformation {
  def apply(pats:List[Pat],decltpe:Type)(implicit context: CollectorContext) : AssociationInformation =
    new AssociationInformation {
    override val pDeclType: TargetTypeCollector = TargetTypeCollector(decltpe)
    override val targetMultiplicity: String = TargetMultiplicityCollector(decltpe).multiplicity
    override val pSources: List[String] = pats.collect { _.syntax }
  }
}
