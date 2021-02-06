package pretty.plantuml

import pretty.KiamaPretty
import pretty.config.PrettyConfig
import pretty.KiamaPretty._
import scalameta.util.namespaces.NamespaceEntry
import uml.GenericParameter

case class ClassPretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[uml.Class] {
  override def toDoc(umlElement: uml.Class): KiamaPretty.Doc = umlElement match {
    case uml.Class(
    isAbstract,
    identifier,
    attributes,
    operations,
    additionalCompartements,
    genericParameters,
    stereotype,
    namespace) =>
      (if (isAbstract) {
        "abstract" <> space
      } else {
        emptyDoc
      }) <>
        "class" <+>
        showNamespace(namespace) <>
        (namespace match {
          case _: NamespaceEntry => "."
          case _ => ""
        }) <>
        identifier <>
        opt(
          genericParameters,
          (gps: List[GenericParameter]) => hsep(gps.map(GenericParameterPretty().toDoc), sep = ','),
          l = "< ",
          r = " >" <> space,
          emptyR = space) <>
        showStereotype(stereotype) <>
        (if (attributes.nonEmpty || operations.nonEmpty || additionalCompartements.nonEmpty) {
          enclose("{",
            nest(line <>
              vsep(attributes.map(AttributePretty().toDoc)) <>
              (if (attributes.nonEmpty) line else emptyDoc) <>
              vsep(operations.map(OperationPretty().toDoc))
            ) <> space <>
              (if (operations.nonEmpty) line else emptyDoc) <>
              vsep(additionalCompartements.map(CompartmentPretty().toDoc)) <> line,
            "}")
        } else emptyDoc)
  }
}
