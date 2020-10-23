package scalameta

import scala.meta.{Traverser, Tree}
import plantuml._

class PlantUMLCollector(tree:Tree) extends Traverser  {
  
}

object PlantUMLCollector {
  def apply(tree: Tree,op: Tree => UMLElement): PlantUMLCollector = {
    tree.collect {

    }
  }
}
