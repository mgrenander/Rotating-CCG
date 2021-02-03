package edin.ccg

import java.io.File
import scala.io.StdIn.readLine
import edin.ccg.representation.DerivationsLoader
import edin.ccg.representation.category.Category
import edin.ccg.representation.tree._

import scala.io.Source


object MainLCAStats {

  private var loadedTrees = List[TreeNode]()
  private var loadedSpans = List[Option[List[(Int, Int)]]]()

  def main(args:Array[String]) : Unit = {

    assert(args.length <= 2)

    if(args.length == 2){
      loadTrees(args(0))
      loadSpans(args(1))
    }

    var stop = false
    while(!stop){
      val inputParts = readLine("> ").split(" +").toList
      inputParts.head.toLowerCase match {
        case "one" =>
          assert(inputParts.size >= 2)
          val sentId = inputParts.last.toInt
          val origTree = loadedTrees(sentId)
          print_leaf_categories(origTree)
        case "span" =>
          assert(inputParts.size >= 2)
          val sentId = inputParts.last.toInt
          val origTree = loadedTrees(sentId)
          print_leaf_spans(origTree)
        case "all" =>
          for((tree, i) <- loadedTrees.zipWithIndex) {
            println("TODO")
          }
        case "visualise" | "visualize" =>
          assert(inputParts.size >= 2)
          val sentId = inputParts.last.toInt
          val stuff = inputParts.tail.init
          val origTree = loadedTrees(sentId)
          val derivationTypes = if (stuff.nonEmpty) stuff else List[String]("original")
          visualizeTree(origTree, derivationTypes, s"$sentId")
        case "visualise_string" | "visualize_string" =>
          val stuff = inputParts.tail.takeWhile(!_.startsWith("("))
          val derivationTypes = if (stuff.nonEmpty) stuff else List[String]("original")
          val origTree = DerivationsLoader.fromString(inputParts.dropWhile(!_.startsWith("(")).mkString(" "))
          visualizeTree(origTree, derivationTypes, "string")
        case "print_string" =>
          println(loadedTrees(inputParts(1).toInt).toCCGbankString)
        case "grep" =>
          val s = inputParts.tail.mkString(" ")
          for((tree, i) <- loadedTrees.zipWithIndex){
            val sent = tree.words.mkString(" ").toLowerCase
            if(sent.toLowerCase contains s){
              println(s"$i: $sent")
            }
          }
        case "grep_cat" =>
          assert(inputParts.size==2)
          val s = inputParts(1)
          for((tree, i) <- loadedTrees.zipWithIndex){
            if(tree.allNodes.exists(_.category.toString == s)) {
              val sent = tree.words.mkString(" ").toLowerCase
              println(s"$i: $sent")
            }
          }
        case "grep_comb" =>
          assert(inputParts.size==2)
          val s = inputParts(1)
          for((tree, i) <- loadedTrees.zipWithIndex){
            if(tree.allNodes.flatMap(_.getCombinator).exists(_.toString == s)) {
              val sent = tree.words.mkString(" ").toLowerCase
              println(s"$i: $sent")
            }
          }
        case "deps" =>
          assert(inputParts.size == 2)
          val sentId = inputParts(1).toInt
          val origTree = loadedTrees(sentId)
          origTree.depsVisualize()
        case "deps_string" =>
          val origTree = DerivationsLoader.fromString(inputParts.dropWhile(!_.startsWith("(")).mkString(" "))
          origTree.depsVisualize()
        case "exit" | "done" | "quit" =>
          stop=true
        case "" =>
        case _ =>
          System.err.println("unknown command "+inputParts.head)
      }

    }

    println("Done")
  }

  private def print_leaf_categories(origTree:TreeNode):Unit = {
    val leafNodes = origTree.leafs
    for( node <- leafNodes ) {
      println(node.toString())
    }
  }

  private def print_leaf_spans(origTree:TreeNode):Unit = {
    val leafNodes = origTree.leafs
    for( node <- leafNodes ) {
      println(node.span.toString())
    }
  }

  private def visualizeTree(origTree:TreeNode, derivationTypesToShow:List[String], info:String):Unit ={
    lazy val leftBranch = origTree.toLeftBranching
    lazy val rightBranch = origTree.toRightBranching
    lazy val revealBranch = origTree.toRevealingBranching

    val toShow : List[(TreeNode, String)] = if(derivationTypesToShow.head == "all"){
      if(rightBranch == origTree)
        List((revealBranch, "revealing"), (leftBranch, "left"), (rightBranch, "right_and_orig"))
      else
        List((revealBranch, "revealing"), (leftBranch, "left"), (rightBranch, "right"), (origTree, "original"))
    }else{
      derivationTypesToShow.map{
        case "original" | "default" => (origTree, "original")
        case "left"  => (leftBranch, "left")
        case "right"  => (rightBranch, "right")
        case "revealing" | "reveal"  => (revealBranch, "revealing")
      }
    }

    for((node, derivationType) <- toShow){
      node.visualize(s"_____${info}_____${derivationType}______")
    }

  }

  private def loadTrees(fileName:String) : Unit = {
    if(! new File(fileName).exists())
      System.err.println(s"file $fileName doesn't exist")
    loadedTrees = DerivationsLoader.fromFile(fileName).toList
    println(loadedTrees.size+" trees successfully loaded")
  }

  private def loadSpans(fileName:String) : Unit = {
    if(! new File(fileName).exists())
      System.err.println(s"file $fileName doesn't exist")
    val file = Source.fromFile(fileName)
    loadedSpans = file.getLines.map { line =>
      if (line.isBlank) {
        None
      } else {
        Some(line.split("\s+").toList.grouped(2).map{case List(x,y) => (x.toInt, y.toInt)}.toList)
      }
    }.toList
    file.close()
  }

}
