package umlCoverage.dotty

import java.io.{File, FileOutputStream}
import java.nio.file.{Files, Path, Paths}
import net.sourceforge.plantuml.SourceStringReader
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import pretty.config.PlantUMLConfig
import pretty.plantuml.UMLUnitPretty
import scalameta.SourceCollector
import scalameta.util.context.GlobalContext
import uml.Stereotype

import scala.meta.{Source, dialects}
import scala.meta.inputs.Input

class ContextBoundSuite extends AnyFreeSpec with Matchers {

  val path: Path = Paths.get("src","test","resources","assets","dotty","contextBounds.txt")

  "Dotty Reference to Intersectiontypes can be processed to a plantUML png" in {
    val bytes = Files.readAllBytes(path)
    val fileString  = new String(bytes,"UTF-8")
    val vFile = Input.VirtualFile(path.toString,fileString)
    val input = dialects.Scala3(vFile).parse[Source].get

    val globalScope = scalameta.util.namespaces.collector.SourcesCollector(List((input,path.toAbsolutePath.toString)))
    val umlCollector = SourceCollector(input,GlobalContext(globalScope.resultingMap),path.toAbsolutePath.toString)

    val umlUnit = umlCollector.umlUnit

    //Tests, that a class stereotyped def with name F exists that carries the function f with the
    //context bound parameters as a last parameter list equipped with the using stereotype
    umlUnit.count {
      case c: uml.Class =>
        c.stereotype.contains(Stereotype("def",Nil)) &&
        c.operations.exists(o =>
          o.name.equals("f") &&
            o.templateParameter.isDefined &&
            o.templateParameter.get.count(_ =>true) == 2 &&
            o.paramSeq.size == 3 &&
            o.paramSeq.last.size == 3 &&
            o.paramSeq.last.exists{
              p =>p.stereotype.exists(s => s.equals(Stereotype("using",Nil))) && p.paramType.equals("C1<T>")
            } &&
            o.paramSeq.last.exists{
              p => p.stereotype.exists(s => s.equals(Stereotype("using",Nil))) && p.paramType.equals("C2<T>")
            } &&
            o.paramSeq.last.exists{
              p =>p.stereotype.exists(s => s.equals(Stereotype("using",Nil))) &&  p.paramType.equals("C3<U>")
            }
        )
      case _ => false
    } must be(1)

    umlUnit.count {
      case c: uml.Class =>
        c.stereotype.contains(Stereotype("def",Nil)) &&
          c.operations.exists(o =>
            o.name.equals("maximum") &&
              o.templateParameter.isDefined &&
              o.templateParameter.get.count(_ =>true) == 1 &&
              o.paramSeq.size == 2 &&
              o.paramSeq.last.size == 1 &&
              o.paramSeq.last.exists{
                p =>p.stereotype.exists(s => s.equals(Stereotype("using",Nil))) && p.paramType.equals("Ord<T>")
              }
          )
      case _ => false
    } must be(1)

    umlUnit.count {
      case c: uml.Class =>
        c.stereotype.contains(Stereotype("def",Nil)) &&
          c.operations.exists(o =>
            o.name.equals("g") &&
              o.templateParameter.isDefined &&
              o.templateParameter.get.count(_ =>true) == 1 &&
              o.paramSeq.size == 3 &&
              o.paramSeq.last.size == 1 &&
              o.paramSeq.last.exists{
                p =>p.stereotype.exists(s => s.equals(Stereotype("using",Nil))) && p.paramType.equals("C<T>")
              }
          )
      case _ => false
    } must be(1)

    implicit val umlUnitPretty: UMLUnitPretty = UMLUnitPretty()(PlantUMLConfig())

    val reader = new SourceStringReader(umlCollector.umlUnit.pretty)
    val filePath = new File("src/test/scala/assets/out/")

    filePath.mkdirs()

    val fos = new FileOutputStream(new File(filePath.getPath + "/contextBounds.png"))
    val sec = reader.generateImage(fos)

    sec must not be null
  }
}
