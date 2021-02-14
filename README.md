# scala-uml

Obtain UML class diagrams from Scala 2 or Scala 3 source code in a compiler-independent way.

##### Table of Contents

- [What it can](#whatcan)
- [How to Use?](#howuse)
- [How does it work?](#howworks)


##### Quick example:

<table>
<tr>
<th>
Input
<th>
Output
</th>
</tr>
<td>
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
</tr>
</table>
</td>
<td>
<img align="center" src="docs/examples/animals.svg">
</td>
</table>

<a name="whatcan"/>

## What it can do

This project can generate UML class diagrams from Scala programs without being dependent on the scala compiler. 
That means you can obtain class diagrams of work in progress files and also standalone files that would not compile on their own.
However, it is also possible to generate class diagrams to multiple directories of a public github repository.
The project takes into account not only the relationships of the classes but also the package definitions. 

Scala-uml evolved from a master thesis in which stereotypes were inserted for all scala-specific features in the context of a UML profile.

- **Input**: Folder of Scala files or a config file targeting a public github repository
- **Output**: UML class diagram of the files as an svg-image or in the format of class diagrams of [PlantUML](https://plantuml.com/de/class-diagram)

<a name="howuse"/>

## How to use it

At the moment there is no release. However, this project is planned to be a console application and it can allready repond to a few console commands.
To use it you need to get the repository and start the application with `sbt "run <commands>"`.  

<a name="howworks"/>

## How it works
