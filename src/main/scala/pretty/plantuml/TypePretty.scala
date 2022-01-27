package pretty.plantuml

import pretty.KiamaPretty
import pretty.KiamaPretty.{hsep, string}
import pretty.config.PrettyConfig
import uml.{RefName, RefPathQualifier, RefTemplate, Type}

case class TypePretty()(override implicit val config : PrettyConfig)
  extends PlantUMLPrettyPrinter[uml.Type] {
  override def toDoc(umlElement: Type): KiamaPretty.Doc = umlElement match {
    case RefName(name, namespace, metaOrigin) => name
    case RefTemplate(preType, templateTypes) =>
      toDoc(preType) <+>
        "<" <>
        hsep(templateTypes.map(toDoc),",") <>
        ">"
    case RefPathQualifier(path, target) =>
      hsep(path.map(f => string(f)),".") <> string(target)
  }
}
