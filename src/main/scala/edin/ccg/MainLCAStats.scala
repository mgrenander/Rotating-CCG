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
    val topk_categories = List("""NP""", """NP[nb]""", """NP[nb]/N""", """N/N""", """N""")
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
          writeFile("dev.cats", all_loaded_spans)
        case "predict_spans" =>
          assert(inputParts.size >= 2)
          val n = inputParts.last.toInt
          val span_predictor = topk_categories.take(n)
          val predictions = predict_spans(span_predictor)
          writePredictions(s"dev.$n.preds", predictions)
        case "find_spans" =>
          assert(inputParts.size >= 2)
          val s = inputParts.last
          val mention_spans = find_mention_span(s)
          val mention_span_len = mention_spans.length
          println(mention_spans.toString())
          println(s"Found $mention_span_len spans corresponding to $s.")
        case "find_na_spans" =>
          val mention_spans = findNASpans()
          writePredictions("dev.na.spans", mention_spans)
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
      if (spans.isDefined) {
        val nodes = tree.allNodes
        val adj_node_spans = nodes.map(node => (node.span._1, node.span._2 - 1))
        for (span <- spans.get) {
          val findSpan = adj_node_spans.indexOf(span)
          if (findSpan != -1) {
            val span_count = adj_node_spans.count(_ == span)
            if (span_count == 1) {
              loaded_span_categories ::= nodes(findSpan).category.toString
            } else {
              // Make sure we don't double count the parents of UnaryNodes
              val children = nodes(findSpan).children
              if (!(children.nonEmpty && children.map(x => x.span).contains(nodes(findSpan).span))) {
                loaded_span_categories ::= nodes(findSpan).category.toString
              }
            }
          } else {
            loaded_span_categories ::= "NA"
          }
        }
      }
    }
    loaded_span_categories
  }

  private def find_mention_span(cat_string : String) : List[String] = {
    var cat_spans = List[String]()
    for((tree, i) <- loadedTrees.zipWithIndex) {
      val spans = loadedSpans(i)
      breakable {
        if (spans.isEmpty) {
          break
        } else {
          val nodes = tree.allNodes
          val adj_node_spans = nodes.map(node => (node.span._1, node.span._2 - 1))
          for (span <- spans.get) {
            val findSpan = adj_node_spans.indexOf(span)
            if (findSpan != -1 && nodes(findSpan).category.toString == cat_string) {
              cat_spans ::= nodes(findSpan).words.mkString(" ")
            }
          }
        }
      }
    }

    cat_spans
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
        // Make sure we don't double count the parents of UnaryNodes
//        val children = node.children
//        if (!(children.nonEmpty && children.map(x => x.span).contains(node.span))) {
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
//        }
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

  private def findNASpans() : Array[Option[List[(Int, Int)]]] = {
    val na_spans = new Array[Option[List[(Int, Int)]]](loadedTrees.size)
    for (i <- loadedTrees.indices) {
      na_spans(i) = None
    }

    for((tree, i) <- loadedTrees.zipWithIndex) {
      if (i % 1000 == 0) {
        println(s"Processed $i trees")
      }

      val spans = loadedSpans(i)
      if (spans.isDefined) {
        val nodes = tree.allNodes
        val adj_node_spans = nodes.map(node => (node.span._1, node.span._2 - 1))
        for (span <- spans.get) {
          if (adj_node_spans.indexOf(span) == -1) { // Span was not found in tree
            if (na_spans(i).isEmpty) {
              na_spans(i) = Some(List(span))
            } else {
              na_spans(i) = Some(na_spans(i).get ++ List(span))
            }
          }
        }
      }
    }
    na_spans
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
    val totalSpans = spansLen()
    println(s"$totalSpans spans loaded.")
    file.close()
  }

  private def spansLen() : Int = {
    var spansCount = 0
    for (loadedSpan <- loadedSpans) {
      if (loadedSpan.isDefined) {
        spansCount += loadedSpan.get.length
      }
    }
    spansCount
  }
}
