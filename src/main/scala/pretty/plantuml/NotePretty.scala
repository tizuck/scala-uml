package pretty.plantuml

import java.util.UUID.randomUUID

import pretty.Pretty
import pretty.config.PrettyConfig
import uml.Note
import pretty.Pretty._

case class NotePretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[Note] {
  override def toDoc(umlElement: Note): Pretty.Doc = umlElement match {
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
