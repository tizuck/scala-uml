package app.ci

import java.io.File
import scala.meta.Source

case class ParseConfig(
                 verbose : Boolean = false,
                 out : File = new File("."),
                 in : Seq[File] = Nil,
                 textual : Boolean = false,
                 name : String = "default",
                 github : Option[File] = None,
                 filter: Option[String] = None
                 )
