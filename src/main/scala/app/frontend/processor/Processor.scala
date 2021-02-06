package app.frontend.processor

import app.frontend._
import app.frontend.exceptions.{BadInputPathException, ImplementationMissingException}
import uml.UMLUnit

import java.io.{FileNotFoundException, IOException}

trait Processor {
  @throws[IOException]()
  @throws[FileNotFoundException]()
  @throws[BadInputPathException]()
  @throws[ImplementationMissingException]()
  def execute():UMLUnit
}

object Processor {
  def apply(commands:List[Command]):Processor = {
    commands.foldLeft[Processor](EmptyProcessor){
      case (_,h:Help)                                       => HelpProcessor(h)

      case (EmptyProcessor,OutputPath(path))                => UMLDiagramProcessor(outputPath = path)
      case (u:UMLDiagramProcessor,OutputPath(path))         => u.copy(outputPath = path)
      case (g:GithubUMLDiagramProcessor,OutputPath(path))   => g.copy(outputPath = path)

      case (EmptyProcessor,InputPath(path))                 => UMLDiagramProcessor(filesPath = path)
      case (u:UMLDiagramProcessor,InputPath(path))          => u.copy(filesPath = path)
      case (_:GithubUMLDiagramProcessor,InputPath(path))    => UMLDiagramProcessor(filesPath = path)

      case (EmptyProcessor,Verbose())                       => UMLDiagramProcessor(isVerbose = true)
      case (u:UMLDiagramProcessor,Verbose())                => u.copy(isVerbose = true)
      case (g:GithubUMLDiagramProcessor,Verbose())          => g.copy(isVerbose = true)

      case (EmptyProcessor,Name(name))                      =>
        UMLDiagramProcessor("","",isVerbose = false,isTextual = false,name)
      case (u:UMLDiagramProcessor,Name(name))               => u.copy(name = name)
      case (g:GithubUMLDiagramProcessor,Name(name))         => g.copy(name = name)

      case (EmptyProcessor,Github(path))                    =>
        GithubUMLDiagramProcessor("",path,isVerbose = false,isTextual = false)
      case (u:UMLDiagramProcessor,Github(path))             =>
        GithubUMLDiagramProcessor(u.outputPath,
          path,
          u.isVerbose,
          u.isTextual,
          u.name)

      case (g:GithubUMLDiagramProcessor,Github(_))       => g

      case (EmptyProcessor,Textual())                       => UMLDiagramProcessor(isTextual = true)
      case (u:UMLDiagramProcessor,Textual())                => u.copy(isTextual = true)
      case (g:GithubUMLDiagramProcessor,Textual())          => g.copy(isTextual = true)

      case (EmptyProcessor,f:Filter)                  => UMLDiagramProcessor(exclude = Some(f))
      case (u:UMLDiagramProcessor,f:Filter)           => u.copy(exclude = Some(f))
      case (g:GithubUMLDiagramProcessor,f:Filter)     => g.copy(exclude = Some(f))
    }
  }
}
