# scala-uml

Obtain UML class diagrams from Scala 2 or Scala 3 source code in a compiler-independent way.

**Input:** Folder of Scala files or a config referencing directories in a public Github repository. 

**Output:**  Image of a class diagram in the svg format, or a file in the UML class diagram format of the [PlantUML](https://plantuml.com/de/class-diagram) project.

## Quick Example

<table>
<tr>
<th>
animal.scala
</th>
<th>
olm.scala
</th>
</tr>
<tr>
<td>

```Scala
trait Animal {
  val location : String
}

sealed case class Wombat(
  location:String
) extends Animal
```

</td>
<td>

```Scala
package animals.extension

import animals.Animal

sealed case class Olm(
  location:String
) extends Animal
```

</td>
<td>
<img align="center" src="docs/examples/animals.svg">
</td>
</tr>
</table>
