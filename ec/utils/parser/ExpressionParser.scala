package utils.parser

import ec.app.fixedpoint._
//import ec.app..GPNode

import scala.util.parsing.combinator._

object ExpressionParser extends RegexParsers {
  val ident = """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*""".r
  val float = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r

  def expr: Parser[FPNode] = ( sum | term )

  def sum: Parser[FPNode] = product * (
    // Here, we return a function that will collapse the values in the list
      "+" ^^^ { (left:FPNode, right:FPNode) => new Add(left, right)}
    | "-" ^^^ { (left:FPNode, right:FPNode) => new Add(left, new Neg(right))}
  )

  def product: Parser[FPNode] = term * (
      "*" ^^^ { (left:FPNode, right:FPNode) => new Mult(left, right)}
    | "/" ^^^ { (left:FPNode, right:FPNode) => new Mult(left, new Inv(right))}
  )

  // throw away the parentheses
  def parens: Parser[FPNode] = "(" ~> expr <~ ")"

  def unaryMinus: Parser[FPNode] = "-" ~> term ^^ { new Neg(_) }

  def term: Parser[FPNode] = (
      ident ^^ {s => new Variable(s)}
    | float ^^ {s => new Constant(s.toDouble)}
    | parens
    | unaryMinus
  )

  def parse(exprStr: String): FPNode = {
    parseAll(expr, exprStr) match {
      case Success(tree, _) => tree
      case e: NoSuccess => println(e); null
    }
  }

  def parseFile(fileName: String): FPNode = {
    val source = scala.io.Source.fromFile(fileName)
    val lines = source.mkString
    val tree = parse(lines)
    source.close()
    tree
  }
}
