package animals.extension

import animals.Animal

sealed case class Olm(
                       location:String
                     ) extends Animal
