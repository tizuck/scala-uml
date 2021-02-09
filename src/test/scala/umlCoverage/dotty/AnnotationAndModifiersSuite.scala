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

class AnnotationAndModifiersSuite extends AnyFreeSpec with Matchers {
  val path: Path = Paths.get("src","test","resources","assets","dotty","annotationsAndModifiers.txt")

  "Dotty Reference to Intersectiontypes can be processed to a plantUML png" in {
    val bytes = Files.readAllBytes(path)
    val fileString  = new String(bytes,"UTF-8")
    val vFile = Input.VirtualFile(path.toString,fileString)
    val input = dialects.Scala3(vFile).parse[Source].get

    val globalScope = scalameta.util.namespaces.collector.SourcesCollector(List((input,path.toAbsolutePath.toString)))
    val umlCollector = SourceCollector(input,GlobalContext(globalScope.resultingMap),path.toAbsolutePath.toString)

    val umlUnit = umlCollector.umlUnit

    implicit val umlUnitPretty: UMLUnitPretty = UMLUnitPretty()(PlantUMLConfig())

    val reader = new SourceStringReader(umlCollector.umlUnit.pretty)
    val filePath = new File("src/test/scala/assets/out/")

    filePath.mkdirs()

    val fos = new FileOutputStream(new File(filePath.getPath + "/annotationsModifiers.png"))
    val sec = reader.generateImage(fos)

    sec must not be null

    umlUnit.exists{
      case c : uml.Class =>
        c.name.trim.equals("Foo") &&
        c.additionalCompartements.exists(c =>
          c.identifier.get.trim.equals("<<annotated>>") &&
            c.taggedValues.exists(t => t.name.trim.equals("annotations") &&
              t.value.exists(s => s.trim.equals("""[@someAnnotation("foo"),@foo]""")))) &&
          c.additionalCompartements.exists(c =>
            c.identifier.get.trim.equals("<<scalaclass>>") &&
            c.taggedValues.exists(t => t.name.trim.equals("isFinal")) &&
              c.taggedValues.exists(t => t.name.trim.equals("isImplicit")
          )
        )
      case _ => false
    } must be(true)
  }
}
