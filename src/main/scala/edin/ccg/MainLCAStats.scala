package edin.ccg

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.StdIn.readLine
import edin.ccg.representation.DerivationsLoader
import edin.ccg.representation.category.Category
import edin.ccg.representation.tree._

import scala.io.Source
import scala.util.control.Breaks._


object MainLCAStats {

  private var loadedTrees = List[TreeNode]()
  private var loadedSpans = List[Option[List[(Int, Int)]]]()

  def main(args:Array[String]) : Unit = {

    assert(args.length <= 2)

    if(args.length == 2){
      loadTrees(args(0))
      loadSpans(args(1))
    }
    var topk_categories = List("NP", "NP[nb]", "NP[nb]/N", "S/(S\\NP)", "N/N", "N")
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
        case "loaded_span" =>
          assert(inputParts.size >= 2)
          val sentId = inputParts.last.toInt
          val spans = loadedSpans(sentId)
          print_loaded_spans(spans)
        case "span_categories" =>
          val all_loaded_spans = find_all_loaded_spans
          writeFile("test.cats", all_loaded_spans)
        case "predict_spans" =>
          assert(inputParts.size >= 2)
          val n = inputParts.last.toInt
          val span_predictor = topk_categories.take(n)
          val predictions = predict_spans(span_predictor)
          writePredictions(s"test.$n.preds", predictions)
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
    val nodes = origTree.allNodes
    for( node <- nodes ) {
      println(node.toString + " " + node.span.toString())
    }
  }

  private def print_loaded_spans(spans:Option[List[(Int, Int)]]):Unit = {
    if ( spans.isEmpty ) {
      println("None")
    } else {
      println(spans.toString)
    }
  }

  private def find_all_loaded_spans : List[String] = {
    var loaded_span_categories = List[String]()
    for((tree, i) <- loadedTrees.zipWithIndex) {
      if (i%1000 == 0) {
        println(s"Processed $i trees")
      }
      val spans = loadedSpans(i)
      breakable {
        if (spans.isEmpty) {
          break
        } else {
          val nodes = tree.allNodes
          val adj_node_spans = nodes.map(node => (node.span._1, node.span._2 - 1))
          for (span <- spans.get) {
            val findSpan = adj_node_spans.indexOf(span)
            if (findSpan != -1) {
              loaded_span_categories ::= nodes(findSpan).category.toString
            } else {
              loaded_span_categories ::= "NA"
            }
          }
        }
      }
    }
    loaded_span_categories
  }

  def writeFile(filename: String, lines: Seq[String]): Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    for (line <- lines) {
      bw.write(line + " ")
    }
    bw.close()
  }

  private def predict_spans(span_predictor : List[String]) : Array[Option[List[(Int, Int)]]] = {
    val predictions = new Array[Option[List[(Int, Int)]]](loadedTrees.size)
    for (i <- loadedTrees.indices) {
      predictions(i) = None
    }

    for((tree, i) <- loadedTrees.zipWithIndex) {
      if (i%1000 == 0) {
        println(s"Processed $i trees")
      }

      val nodes = tree.allNodes
      for (node <- nodes) {
        // Check if node's category is in span_predictor
        if (span_predictor.contains(node.category.toString)) {
          // Append this span to array at i. Decrement the span endpoint due to formatting.
          val predicted_span = (node.span._1, node.span._2 - 1)
          if (predictions(i).isEmpty) {
            predictions(i) = Some(List(predicted_span))
          } else {
            predictions(i) = Some(predictions(i).get ++ List(predicted_span))
          }
        }
      }
    }
    predictions
  }

  private def writePredictions(filename : String, predictions : Array[Option[List[(Int, Int)]]]) : Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    for (prediction <- predictions) {
      if (prediction.isEmpty) {
        bw.write("\n")
      } else {
        bw.write(prediction.get.toString() + "\n")
      }
    }
    bw.close()
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
    loadedSpans = file.getLines().map { line =>
      if (line.isEmpty) {
        None
      } else {
        Some(line.split(" +").toList.grouped(2).map{case List(x,y) => (x.toInt, y.toInt)}.toList)
      }
    }.toList
    file.close()
  }

}
