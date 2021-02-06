package pretty.plantuml

import pretty.KiamaPretty
import pretty.config.PrettyConfig
import uml.{FromTo, Relationship, RelationshipInfo, ToFrom}
import pretty.KiamaPretty._

case class RelationshipPretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[Relationship] {
  override def toDoc(umlElement: Relationship): KiamaPretty.Doc = umlElement match {
    case Relationship(
    relationshipType,
    relationshipDirection,
    RelationshipInfo(
      sourceMultiplicity,
      targetMultiplicity,
      from,
      to,
      relationshipIdentifier,
      identifierDirection,
      _
    ),
    stereotype) =>
      RelationshipElementPretty().toDoc(from) <+>
        opt(sourceMultiplicity, (s:String) => surround(text(s),'"')) <>
        showRelationshipConnector(relationshipType,relationshipDirection) <+>
        opt(targetMultiplicity, (s:String) => surround(text(s),'"')) <>
        RelationshipElementPretty().toDoc(to) <+>
        (if(relationshipIdentifier.isDefined || stereotype.nonEmpty) {
          ":" <+>
            (if (identifierDirection.equals(ToFrom)) {
              "<" <> space
            } else {
              emptyDoc
            }) <>
            showStereotype(stereotype) <>
            opt(relationshipIdentifier, text)  <>
            (if (identifierDirection.equals(FromTo)) {
              space <> ">" <> space
            } else {
              emptyDoc
            })
        } else {
          emptyDoc
        })

  }
}
