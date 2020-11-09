package scalameta.util

import scala.meta.Defn.{Class, Object, Trait, Type}
import scala.meta.{Pkg, Stat}

object util {
  def statToString(stat:Stat):String = stat match {
    case Pkg(ref, value) => s"""Pkg(${ref.structure})"""
    case Class(_,n,_,_,_) => s"Class($n)"
    case Trait(_,n,_,_,_) => s"Trait($n)"
    case Type(_,n,_,_) => s"Type($n)"
    case Object(value, name, template) => s"Object($name)"
  }
}
