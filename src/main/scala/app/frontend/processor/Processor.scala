package app.frontend.processor

import app.frontend._
import uml.UMLUnit

import java.io.{FileNotFoundException, IOException}

trait Processor {
  @throws[IOException]()
  @throws[FileNotFoundException]()
  def execute():UMLUnit
}

object Processor {
  def apply(commands:List[Command]):Processor = {
    commands.foldLeft[Processor](EmptyProcessor){
      case (_,h:Help)                                       => HelpProcessor(h)

      case (EmptyProcessor,OutputPath(path))                => UMLDiagramProcessor(outputPath = path,"",false,false)
      case (u:UMLDiagramProcessor,OutputPath(path))         => u.copy(outputPath = path)
      case (g:GithubUMLDiagramProcessor,OutputPath(path))   => g.copy(outputPath = path)

      case (EmptyProcessor,InputPath(path))                 => UMLDiagramProcessor(outputPath = "",filesPath = path,false,false)
      case (u:UMLDiagramProcessor,InputPath(path))          => u.copy(filesPath = path)
      case (g:GithubUMLDiagramProcessor,InputPath(path))    => UMLDiagramProcessor(outputPath = "",filesPath = path,false,false)

      case (EmptyProcessor,Verbose())                       => UMLDiagramProcessor("","",true,false)
      case (u:UMLDiagramProcessor,Verbose())                => u.copy(isVerbose = true)
      case (g:GithubUMLDiagramProcessor,Verbose())          => g.copy(isVerbose = true)

      case (EmptyProcessor,Name(name))                      => UMLDiagramProcessor("","",false,false,name)
      case (u:UMLDiagramProcessor,Name(name))               => u.copy(name = name)
      case (g:GithubUMLDiagramProcessor,Name(name))         => g.copy(name = name)

      case (EmptyProcessor,Github(path))                    => GithubUMLDiagramProcessor("",path,false,false)
      case (u:UMLDiagramProcessor,Github(path))             =>
        GithubUMLDiagramProcessor(u.outputPath,
          path,
          u.isVerbose,
          u.isTextual,
          u.name)

      case (g:GithubUMLDiagramProcessor,Github(_))       => g

      case (EmptyProcessor,Textual())                       => UMLDiagramProcessor("","",false,true)
      case (u:UMLDiagramProcessor,Textual())                => u.copy(isTextual = true)
      case (g:GithubUMLDiagramProcessor,Textual())          => g.copy(isTextual = true)

      case (EmptyProcessor,f:Filter)                  => UMLDiagramProcessor("","",false,false,exclude = Some(f))
      case (u:UMLDiagramProcessor,f:Filter)           => u.copy(exclude = Some(f))
      case (g:GithubUMLDiagramProcessor,f:Filter)     => g.copy(exclude = Some(f))
    }
  }
}
