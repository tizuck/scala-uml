package app.frontend2

import java.io.File
import scala.meta.Source

case class Config(
                 verbose : Boolean = false,
                 out : File = new File("."),
                 in : Seq[(Source,String)] = Nil,
                 textual : Boolean = false,
                 name : String = "default",
                 github : Option[File] = None
                 )
