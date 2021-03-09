package edin.ccg.representation

import java.io.File

import edin.ccg.representation.category.Category
import edin.ccg.representation.combinators._
import edin.ccg.representation.tree._

import scala.io.Source

object DerivationsLoader {

  def fromFile(trees_file:String) : Iterator[TreeNode] = {
    fromFile(new File(trees_file))
  }

  private def assignSpans(root:TreeNode) : Unit = {
    root.leafs.zipWithIndex.foreach{case (n, i) => n.position = i}
    root.span
  }

  def fromFile(trees_file:File) : Iterator[TreeNode] = {
    var loaded = 0
    Source.fromFile(trees_file).getLines().flatMap{ line =>
      var resultingTree:Option[TreeNode] = None
      if( line.startsWith("(") ){
        try{
          val tree = this.fromString(line)
          assignSpans(tree)
          resultingTree = Some(tree)
          loaded += 1
          if(loaded % 1000 == 0){
            System.err.println(loaded)
          }
        }catch{
          case e:Exception => System.err.println(s"failed to load this tree #$loaded so i'll skip it: $line\n$e")
        }
      }
      resultingTree
    }
  }

  def fromString(s:String) : TreeNode = {
    val tokens = tokenize(s)
    val (node, consumedTokensCount, _) = processTokens(tokens, 0, 0)
    assert(consumedTokensCount == tokens.length)
    node.leafs.zipWithIndex.foreach{case (n, i) => n.position=i}
    node
  }

  private def processTokens(tokens:Array[String], i:Int, j:Int) : (TreeNode, Int, Int) = {
    var next_token = i
    var next_word = j
    assert(tokens(next_token) == "("); next_token += 1
    assert(tokens(next_token) == "<"); next_token += 1
    tokens(next_token) match {
      case "T" =>
        next_token += 1
        val category:Category = Category.fromString(tokens(next_token)); next_token += 1
        val head_index = tokens(next_token); next_token += 1
        val head_left = head_index == "0"
        val children_count = tokens(next_token); next_token += 1
        assert(tokens(next_token) == ">"); next_token += 1

        var children = List[TreeNode]()

        while(tokens(next_token) == "(") {
          val (child, nextyTokeny, nextyWordy) = processTokens(tokens, next_token, next_word)
          next_token = nextyTokeny
          next_word = nextyWordy
          children ::= child
        }

        children = children.reverse
        assert(tokens(next_token)==")")
        assert(children.size <= 2)

        val node = children match {
          case List(x, y) =>
            val combinator = recognizeCCGcombinatorBinary(x, y, head_left, category)
            BinaryNode(combinator, x, y)
          case List(x) =>
            val combinator = recognizeCCGcombinatorUnary(x.category, category)
            val p = UnaryNode(combinator, x)
            if(p.category != x.category) // this is needed because of the error in the treebank
              p
            else
              x
          case _ =>
            throw new Exception("Input parsing error")
        }

        next_token += 1
        (node, next_token, next_word)
      case "L" =>
        next_token += 1
        val category = Category.fromString(tokens(next_token)); next_token += 1
        val modified_pos = tokens(next_token); next_token += 1
        val original_pos = tokens(next_token); next_token += 1
        val word = tokens(next_token); next_token += 1
        val pred_arg_CCG = tokens(next_token); next_token += 1
        assert(tokens(next_token) == ">"); next_token += 1
        assert(tokens(next_token) == ")"); next_token += 1
        val node = TerminalNode(word, category) // (next_word)
        next_word += 1
        (node, next_token, next_word)
      case _ =>
        throw new Exception("unkown node type")
    }
  }


  private def tokenize(s:String) : Array[String] = {
    val s_tmp = s.replaceAll("<"," < ").replaceAll(">"," > ")
    val reg_str = "X (([:\\/'`$%,\\-.A-Za-z0-9]* )+)X"
    val reg_exp = reg_str.r
    val parts = s_tmp.split(reg_str).toList
    val ne_arr = reg_exp.findAllIn(s_tmp).toList.map(manipulateNEString)
    val assembled = intersperse(parts, ne_arr).mkString(" ")
    assembled.trim().split(" +")
  }

  private def manipulateNEString(x : String) : String = {
    x.replaceAll(" ", "-")
      .substring(4, x.length - 2)
      .concat(" X").reverse.concat(" X X").reverse
  }

  def intersperse(a : List[String], b : List[String]): List[String] = a match {
    case first :: rest => first :: intersperse(b, rest)
    case _             => b
  }

  private def recognizeCCGcombinatorUnary(childCat:Category, parentCat:Category) : CombinatorUnary =
    CombinatorUnary.allPredefined
                   .find(c => c.canApply(childCat) && c(childCat) == parentCat)
                   .getOrElse(TypeChangeUnary(childCat, parentCat))

  private def recognizeCCGcombinatorBinary(x:TreeNode, y:TreeNode, headLeft:Boolean, category:Category) : CombinatorBinary = {
    if(category.toString == "GLUE"){
      return Glue()
    }
    val combinators = CombinatorBinary.allPredefined
    val xCat = x.category
    val yCat = y.category
    if(CombinatorBinary.puncLeft.canApply(xCat, yCat) && CombinatorBinary.puncLeft(xCat, yCat) == category){
      return CombinatorBinary.puncLeft
    }
    for(combinator <- combinators){
      if(combinator.canApply(xCat, yCat)){
        if(combinator(xCat, yCat) == category){
          return combinator
        }
      }
    }
    val combinator = TypeChangeBinary(xCat, yCat, category)
    return combinator
  }


}


