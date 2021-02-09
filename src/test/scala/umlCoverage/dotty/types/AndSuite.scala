package umlCoverage.dotty.types

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

class AndSuite extends AnyFreeSpec with Matchers {

  val path: Path = Paths.get("src","test","resources","assets","dotty","and","and.txt")

  "Dotty Reference to Intersectiontypes can be processed to a plantUML png" in {
    val bytes = Files.readAllBytes(path)
    val fileString  = new String(bytes,"UTF-8")
    val vFile = Input.VirtualFile(path.toString,fileString)
    val input = dialects.Scala3(vFile).parse[Source].get

    val globalScope = scalameta.util.namespaces.collector.SourcesCollector(List((input,path.toAbsolutePath.toString)))
    val umlCollector = SourceCollector(input,GlobalContext(globalScope.resultingMap),path.toAbsolutePath.toString)

    implicit val umlUnit: UMLUnitPretty = UMLUnitPretty()(PlantUMLConfig())
    val reader = new SourceStringReader(umlCollector.umlUnit.pretty)
    val filePath = new File("src/test/scala/assets/out/and/")

    umlCollector.umlUnit.exists{
      case a:uml.Attribute => a.attributeType.exists(s => s.equals("&<A,B>")) && a.name.equals("x")
      case _ => false
    }

    filePath.mkdirs()

    val fos = new FileOutputStream(new File(filePath.getPath + "/dottyAnd.png"))
    val sec = reader.generateImage(fos)

    sec must not be null
  }
}
