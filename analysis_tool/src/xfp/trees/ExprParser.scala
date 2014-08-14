package xfp.trees

import scala.util.parsing.combinator._

object ExprParser extends RegexParsers {
  val ident = """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*"""r
  val float = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r

  def expr: Parser[CExpr] = ( sum | term )

  def sum: Parser[CExpr] = product * (
    // Here, we return a function that will collapse the values in the list
      "+" ^^^ { (left:CExpr, right:CExpr) => CAdd(left, right)}
    | "-" ^^^ { (left:CExpr, right:CExpr) => CSub(left, right)}
  )

  def product: Parser[CExpr] = term * (
      "*" ^^^ { (left:CExpr, right:CExpr) => CMult(left, right)}
    | "/" ^^^ { (left:CExpr, right:CExpr) => CMult(left, CInv(right))}
  )

  // throw away the parentheses
  def parens: Parser[CExpr] = "(" ~> expr <~ ")"

  def unaryMinus: Parser[CExpr] = "-" ~> term ^^ { CNeg(_) }

  def term: Parser[CExpr] = (
      ident ^^ {s => CVar(s)}
    | float ^^ {s => CDoubleConst(s.toDouble)}
    | parens
    | unaryMinus
  )

  def parse(exprStr: String): CExpr = {
    parseAll(expr, exprStr) match {
      case Success(tree, _) => tree
      case e: NoSuccess => println(e); null
    }
  }

  def parseFile(fileName: String): CExpr = {
    val source = scala.io.Source.fromFile(fileName)
    val lines = source.mkString
    val tree = parse(lines)
    source.close()
    tree
  }
}
