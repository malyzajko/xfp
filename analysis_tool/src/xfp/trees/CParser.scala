package xfp.trees

import scala.util.parsing.combinator._

object CParser extends RegexParsers {
  val ident = """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*"""r
  //val integer = """[1-9][0-9]*"""r
  val float = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r

  def expr: Parser[CExpr] = ( sum | term )

  def sum: Parser[CExpr] = product * (
    // Here, we return a function that will collapse the values in the list
      "+" ^^^ { (left:CExpr, right:CExpr) => CAdd(left, right)}
    | "-" ^^^ { (left:CExpr, right:CExpr) => CSub(left, right)}
  )

  def product: Parser[CExpr] = term * (
      "*" ^^^ { (left:CExpr, right:CExpr) => CMult(left, right)}
    | "/" ^^^ { (left:CExpr, right:CExpr) => CMult(left, CInv(right))} // CDiv makes trouble
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

  def member: Parser[CStmt] = (
      "float" ~ ident ~ "=" ~ expr ~ ";" ^^ { case "float" ~ v ~ "=" ~ e ~ ";" => CAssignment(CVar(v), e) }
    | "float" ~ ident ~ ";" ^^ { case "float" ~ v ~ ";" => CDeclaration(CVar(v)) }
    | ident ~ "=" ~ expr ~ ";" ^^ { case v ~ "=" ~ e ~ ";" => CAssignment(CVar(v), e) }
  )

  def function: Parser[CFunction] = "void" ~ ident ~ "(void)" ~ "{" ~ basicBlock ~ "}" ^^ {
    case "void" ~ name ~ "(void)" ~ "{" ~ members ~ "}" => CFunction(name, members)
  }

  def basicBlock: Parser[CBasicBlock] = rep(member) ^^ { m: List[CStmt] => CBasicBlock(m) }

  def program: Parser[CProgram] = basicBlock ~ (function*) ^^ {
    case decls ~ fncs => CProgram(decls, fncs)
  }

  def code: Parser[CTree] = ( function | program )

  def parse(exprStr: String): CTree = {
    parseAll(code, exprStr) match {
      case Success(tree, _) =>
        tree
      case e: NoSuccess =>
        println(e);
        null
    }
  }

  def parseFile(fileName: String): CTree = {
    val source = scala.io.Source.fromFile(fileName)
    val lines = source.mkString
    val tree = parse(lines)
    source.close()
    tree
  }
}
