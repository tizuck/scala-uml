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
import scala.meta.{Source, dialects}
import scala.meta.inputs.Input

class TemplateParameterSuite extends AnyFreeSpec with Matchers {

  val path: Path = Paths.get("src","test","resources","assets","dotty","template.txt")

  "Scala 3 Reference to Intersectiontypes can be processed to a plantUML png" in {
    val bytes = Files.readAllBytes(path)
    val fileString = new String(bytes, "UTF-8")
    val vFile = Input.VirtualFile(path.toString, fileString)
    val input = dialects.Scala3(vFile).parse[Source].get

    val globalScope = scalameta.util.namespaces.collector.SourcesCollector(List((input, path.toAbsolutePath.toString)))
    val umlCollector = SourceCollector(input, GlobalContext(globalScope.resultingMap), path.toAbsolutePath.toString)


    implicit val pretty: UMLUnitPretty = UMLUnitPretty()(PlantUMLConfig())

    umlCollector.umlUnit.exists{
      case c:uml.Class =>
        c.name.equals("A") &&
        c.stereotype.exists(s => s.name.equals("trait")) &&
        c.genericParameters.exists(gs => gs.exists(g => g.name.equals("A") && g.stereotype.exists(s => s.name.equals("+"))))
      case _ => false
    } must be(true)

    umlCollector.umlUnit.exists{
      case c:uml.Class =>
        c.name.equals("B") &&
          c.stereotype.exists(s => s.name.equals("trait")) &&
          c.genericParameters.exists(gs => gs.exists(g => g.name.equals("B") && g.stereotype.exists(s => s.name.equals("-"))))
      case _ => false
    } must be(true)

    umlCollector.umlUnit.exists{
      case c:uml.Class =>
        c.name.equals("C") &&
          c.stereotype.exists(s => s.name.equals("trait")) &&
          c.genericParameters.exists(gs => gs.exists(g => g.name.equals("A") && g.concreteType.exists(s => s.equals("HigherBound<B>"))))
      case _ => false
    } must be(true)

    umlCollector.umlUnit.exists{
      case c:uml.Class =>
        c.name.equals("D") &&
          c.stereotype.exists(s => s.name.equals("trait")) &&
          c.genericParameters.exists(gs => gs.exists(g => g.name.equals("A") && g.concreteType.exists(s => s.equals("LowerBound<B>"))))
      case _ => false
    } must be(true)


    val reader = new SourceStringReader(umlCollector.umlUnit.pretty)
    val filePath = new File("src/test/scala/assets/out/")

    filePath.mkdirs()

    val fos = new FileOutputStream(new File(filePath.getPath + "/templateParams.png"))
    val sec = reader.generateImage(fos)

    sec must not be null
  }
}
