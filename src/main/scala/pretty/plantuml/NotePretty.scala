package pretty.plantuml

import java.util.UUID.randomUUID

import pretty.KiamaPretty
import pretty.config.PrettyConfig
import uml.Note
import pretty.KiamaPretty._

case class NotePretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[Note] {
  override def toDoc(umlElement: Note): KiamaPretty.Doc = umlElement match {
    case Note(attachedElements, nText, stereotype) =>
      val noteId = randomUUID().toString

      "note" <+>
        surround('"', nText) <+>
        "as" <+>
        noteId <>
        (if (attachedElements.nonEmpty) {
          line <>
            vsep(attachedElements.map(NamedElementPretty().toDoc))
        } else {
          emptyDoc
        })
  }
}
