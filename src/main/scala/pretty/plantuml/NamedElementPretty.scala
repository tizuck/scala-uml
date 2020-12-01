package pretty.plantuml

import pretty.KiamaPretty
import pretty.config.PrettyConfig
import uml.{Attribute, GenericParameter, NamedElement, Operation, Parameter}

case class NamedElementPretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[NamedElement] {
  override def toDoc(umlElement: NamedElement): KiamaPretty.Doc = umlElement match {
    case g:GenericParameter   =>  GenericParameterPretty().toDoc(g)
    case c:uml.Class          =>  ClassPretty().toDoc(c)
    case a:Attribute          =>  AttributePretty().toDoc(a)
    case p:Parameter          =>  ParameterPretty().toDoc(p)
    case o:Operation          =>  OperationPretty().toDoc(o)
  }
}
