package scalameta.stats.util

import scalameta.stateless.{TargetMultiplicityCollector, TypeNameCollector}
import scalameta.util.CollectorContext

import scala.meta.{Pat, Type}

trait AssociationInformation {
  val pDeclType : String
  val targetMultiplicity : String
  val pSources : List[String]
}

object AssociationInformation {
  def apply(pats:List[Pat],decltpe:Type)(implicit context: CollectorContext) : AssociationInformation =
    new AssociationInformation {
    override val pDeclType: String = TypeNameCollector(decltpe).typeRep
    override val targetMultiplicity: String = TargetMultiplicityCollector(decltpe).multiplicity
    override val pSources: List[String] = pats.collect { _.syntax }
  }
}