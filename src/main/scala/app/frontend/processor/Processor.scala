package app.frontend.processor

import app.frontend._

import java.io.IOException

trait Processor {
  @throws(classOf[IOException])
  def execute():Unit
}

object Processor {
  def apply(commands:List[Command]):Processor = {
    commands.foldLeft[Processor](EmptyProcessor){
      case (_,h:Help)                               => HelpProcessor(h)

      case (EmptyProcessor,Directory(path))         => UMLDiagramProcessor(outputPath = path,"",false)
      case (u:UMLDiagramProcessor,Directory(path))  => u.copy(outputPath = path)

      case (EmptyProcessor,FilesPath(path))         => UMLDiagramProcessor(outputPath = "",filesPath = path,false)
      case (u:UMLDiagramProcessor,FilesPath(path))  => u.copy(filesPath = path)

      case (EmptyProcessor,Verbose())               => UMLDiagramProcessor("","",true)
      case (u:UMLDiagramProcessor,Verbose())        => u.copy(isVerbose = true)

      case (EmptyProcessor,Name(name))              => UMLDiagramProcessor("","",false,name)
      case (u:UMLDiagramProcessor,Name(name))       => u.copy(name = name)
    }
  }
}
