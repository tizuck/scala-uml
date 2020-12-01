package pretty.plantuml
import pretty.KiamaPretty
import pretty.config.PrettyConfig
import uml.{GenericParameter, Operation}
import pretty.KiamaPretty._

case class OperationPretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[Operation] {
  override def toDoc(umlElement: Operation): KiamaPretty.Doc = umlElement match {
    case Operation(
    modificators,
    accessModifier,
    identifier,
    paramSeq,
    returnType,
    stereotype,
    templateParameter) =>
      showStereotype(stereotype) <>
        opt(modificators,showModificators) <>
        opt(accessModifier,showAccessModifier) <>
        identifier <>
        opt(templateParameter,  (gps:List[GenericParameter]) => hsep(gps.map(GenericParameterPretty().toDoc),sep = ','),l="< ",r=" >" <> space,emptyR = space) <>
        hsep(paramSeq.map(params => '(' <> hsep(params.map(ParameterPretty().toDoc),", ") <> ')')) <+>
        opt(returnType,text,":" <> space,r = emptyDoc)
  }
}
