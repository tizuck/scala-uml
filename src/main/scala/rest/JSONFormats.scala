package rest

import spray.json.{DefaultJsonProtocol, RootJsonFormat}
import ASTCreator._

object JSONFormats {
  import DefaultJsonProtocol._

  implicit val scalaFileEntryJson: RootJsonFormat[ScalaFileEntry] =
    jsonFormat2(ScalaFileEntry)
  implicit val scalaFilesCollectionJson: RootJsonFormat[ScalaFilesCollection] =
    jsonFormat1(ScalaFilesCollection)

  implicit val astCreatedJson: RootJsonFormat[ASTCreated] =
    jsonFormat1(ASTCreated)
}
