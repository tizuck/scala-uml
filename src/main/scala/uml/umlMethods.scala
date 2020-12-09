package uml

import cats.Eval
import cats.data.State
import scalameta.util.namespaces.Entry
import uml.UMLElement
import uml.strategies.collecting.CollectStrategy
import uml.strategies.collecting.packagerep.{CollectAllClassesStrat, CollectAllNamespacesStrat, CollectNamespaceObjectsStrat}
import uml.strategies.rewriting.RewriteStrategy
import uml.strategies.rewriting.packagerep.{DeleteAllClassesOnToplevel, DeleteEmptyPackages, DeleteInnerAssocStrat, InsertClassesInPackageStrat, InsertInnerNamespaceRelsStrat, InsertPackagesFromNamespacesStrat}

object umlMethods {

  private def startState[T](start:T):State[UMLElement,T] = State(
    umlElem => (umlElem,start)
  )

  private def nextRewriteState[T](start:T)(rewriteStrategy: RewriteStrategy[T]):State[UMLElement,T] = State(
    umlElem => (umlElem.rewrite(rewriteStrategy)(start)((_,t) => t).value._2,start)
  )

  import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._

  private def nextCollectState[T](start:T)(collectStrategy: CollectStrategy[T]):State[UMLElement,T] = State(
    umlElem => (umlElem,umlElem.rewrite((_:T) => id)(start)(collectStrategy).value._1)
  )

  private val transferToPackageRep: State[UMLElement,List[uml.Class]] =
    for {
      start <- startState(List.empty[uml.Class])
      collected <- nextCollectState(start)(CollectNamespaceObjectsStrat)
      _ <- nextRewriteState(collected)(DeleteInnerAssocStrat)
      namespace <- nextCollectState(List.empty[Entry])(CollectAllNamespacesStrat)
      _ <- nextRewriteState(namespace)(InsertPackagesFromNamespacesStrat)
      allClasses <- nextCollectState(List.empty[uml.Class])(CollectAllClassesStrat)
      _ <- nextRewriteState(allClasses)(InsertClassesInPackageStrat)
      _ <- nextRewriteState(allClasses)(DeleteAllClassesOnToplevel)
      _ <- nextRewriteState(())(DeleteEmptyPackages)
      res <- nextRewriteState(collected)(InsertInnerNamespaceRelsStrat)
    } yield {
      res
    }

  def toPackageRep(umlElement: UMLElement): Eval[UMLElement] =
    transferToPackageRep.runS(umlElement)
}
