package umlCoverage.dotty

import net.sourceforge.plantuml.SourceStringReader
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import pretty.config.PlantUMLConfig
import pretty.plantuml.UMLUnitPretty
import scalameta.SourceCollector
import scalameta.util.context.GlobalContext

import java.io.{File, FileOutputStream}
import java.nio.file.{Files, Path, Paths}
import scala.meta.inputs.Input
import scala.meta.{Source, dialects}

class OpenClassesSuite extends AnyFreeSpec with Matchers {

  val path: Path = Paths.get("src","test","resources","assets","dotty","openclasses.txt")

  "Dotty Reference to enums can be processed to a plantUML png" in {
    val bytes = Files.readAllBytes(path)
    val fileString  = new String(bytes,"UTF-8")
    val vFile = Input.VirtualFile(path.toString,fileString)
    val input = dialects.Scala3(vFile).parse[Source].get

    val globalScope = scalameta.util.namespaces.collector.SourcesCollector(List((input,path.toAbsolutePath.toString)))
    val umlCollector = SourceCollector(input,GlobalContext(globalScope.resultingMap),path.toAbsolutePath.toString)

    implicit val umlUnit: UMLUnitPretty = UMLUnitPretty()(PlantUMLConfig())

    val reader = new SourceStringReader(umlCollector.umlUnit.pretty)
    val filePath = new File("src/test/scala/assets/out/")

    umlCollector.umlUnit.exists{
      case c:uml.Class =>
        c.name.equals("Writer") &&
        c.additionalCompartements.exists(
          ac => ac.identifier.exists(_.equals("<<scalaclass>>")) &&
            ac.taggedValues.exists(t => t.name.equals("isOpen"))
        )
      case _ => false
    } must be(true)

    filePath.mkdirs()

    val fos = new FileOutputStream(new File(filePath.getPath + "/openclasses.png"))
    val sec = reader.generateImage(fos)

    sec must not be null
  }
}
