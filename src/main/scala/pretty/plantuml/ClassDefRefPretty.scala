package pretty.plantuml

import pretty.KiamaPretty
import pretty.config.PrettyConfig
import uml.{GenericParameter, Stereotype}
import uml.externalReferences.ClassDefRef

case class ClassDefRefPretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[ClassDefRef] {
  override def toDoc(umlElement: ClassDefRef): KiamaPretty.Doc = umlElement.classtype match {
    case uml.externalReferences.Trait =>
      val cls = uml.Class(isAbstract = true, umlElement.name, Nil, Nil, Nil,
        Option.when(umlElement.templateParameter.nonEmpty)(umlElement.templateParameter.map(s => uml.GenericParameter(s,None,Nil))),
        List(uml.Stereotype("trait",Nil)), umlElement.namespace
      )
      ClassPretty().toDoc(cls)
    case uml.externalReferences.Enum =>
      val cls = uml.Class(isAbstract = false, umlElement.name, Nil, Nil, Nil,
        Option.when(umlElement.templateParameter.nonEmpty)(umlElement.templateParameter.map(s => GenericParameter(s,None,Nil))),
        List(Stereotype("enum",Nil)),umlElement.namespace
      )
      ClassPretty().toDoc(cls)
    case uml.externalReferences.Object =>
      val cls = uml.Class(isAbstract = false, umlElement.name, Nil, Nil, Nil, None, List(Stereotype("object",Nil)), umlElement.namespace)
      ClassPretty().toDoc(cls)
    case uml.externalReferences.CClass =>
      val cls = uml.Class(isAbstract = false, umlElement.name, Nil, Nil, Nil,
        Option.when(umlElement.templateParameter.nonEmpty)(umlElement.templateParameter.map(s => GenericParameter(s,None,Nil))),
        Nil, umlElement.namespace
      )
      ClassPretty().toDoc(cls)
    case uml.externalReferences.CCaseClass =>
      val cls = uml.Class(isAbstract = false, umlElement.name, Nil, Nil, Nil,
        Option.when(umlElement.templateParameter.nonEmpty)(umlElement.templateParameter.map(s => GenericParameter(s,None,Nil))),
        List(Stereotype("caseclass",Nil)), umlElement.namespace)
      ClassPretty().toDoc(cls)
  }
}
