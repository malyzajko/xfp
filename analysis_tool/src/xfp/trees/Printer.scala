package xfp.trees

import xfp.fixedpoint.{FixedPointFormat}

object Printer {

  def printResults(ctree: CTree): Unit = ctree match {
    case CProgram(g, fncs) =>
      fncs.foreach { fnc => println("\n" + fnc.name); printResults(fnc) }
    case f: CFunction =>
      for ((key, value) <- f.varMap)
        if (value != null) println("%s:\t%s".format(key, value.toString))
        else println("%s: -".format(key))
    case _ =>;
  }

  def printResults(ctree: CTree, name: String): Unit = ctree match {
    case CProgram(g, fncs) =>
      fncs.foreach { fnc => println("\n" + fnc.name); printResults(fnc, name) }
    case f: CFunction =>
      println("%s: %s".format(name, f.varMap(name).toString))
    case _ =>;
  }

  var counter = 0
  // printTreeWithFormats
  def pTWF(ctree: CExpr): String = ctree match {
    case CVar(name) => name + "{"+ctree.globalFormat.f+"}"
    case CDoubleConst(value) => value + "{"+ctree.globalFormat.f+"}"
    case CNeg(rhs) => "-(%s){%d}".format(pTWF(rhs), ctree.globalFormat.f)
    case CAdd(lhs, rhs) => "(%s + %s){%d}".format(pTWF(lhs), pTWF(rhs), ctree.globalFormat.f)
    case CSub(lhs, rhs) => "(%s - %s){%d}".format(pTWF(lhs), pTWF(rhs), ctree.globalFormat.f)
    case CMult(lhs, rhs) => "(%s * %s){%d}".format(pTWF(lhs), pTWF(rhs), ctree.globalFormat.f)
    case CInv(rhs) => "(1/%s){%d}".format(pTWF(rhs), ctree.globalFormat.f)
    case CDiv(lhs, rhs) => "(%s / %s){%d}".format(pTWF(lhs), pTWF(rhs), ctree.globalFormat.f)
  }

  def getC(tree: FTree): String = tree match {
    case FProgram(g, fncs) =>
      fncs.foldLeft(getC(g)) { (acc, x) => acc + "\n" + getC(x) }
    case f: FFunction => //(name, FBasicBlock(stmts)) =>
      "\nvoid %s (void) {%s\n}".format(f.name,
       f.members.stmts.foldLeft("") { (acc, x) => acc + "\n  " + getC(x, f.varMap)} )
    case FBasicBlock(stmts) =>
      stmts.foldLeft("") { (acc, x) => acc + "\n  " + getC(x)}
    case FAssignment(lhs, rhs) =>
      "%s = (short int)(%s);".format(getC(lhs), getC(rhs))
    case FDeclaration(variable) =>
      "short int %s;".format(getC(variable))
    case FLongConst(value) => value.toString
    case FVar(name) => name
    case FNeg(rhs) => "-(%s)".format(getC(rhs))
    case FAdd(lhs, rhs) => "(%s + %s)".format(getC(lhs), getC(rhs))
    case FSub(lhs, rhs) => "(%s - %s)".format(getC(lhs), getC(rhs))
    case FMult(lhs, rhs) => "(%s * %s)".format(getC(lhs), getC(rhs))
    case FDiv(lhs, rhs) => "(%s / %s)".format(getC(lhs), getC(rhs))
    case FRightShift(expr, bits) =>"(%s >> %d)".format(getC(expr), bits)
    case FLeftShift(expr, bits) =>"(%s << %d)".format(getC(expr), bits)
  }

  def getC(stmt: FStmt, formats: collection.mutable.Map[String, FixedPointFormat]): String = stmt match {
    case FAssignment(lhs, rhs) =>
      "%s = (short int)(%s);\t\t//%s".format(getC(lhs), getC(rhs), formats(lhs.name).toString)
    case FDeclaration(variable) =>
      "short int %s;".format(getC(variable))
  }

  def getScala(tree: FTree, paramNames: Iterable[String]): String = tree match {
    case FProgram(g, fncs) =>
      fncs.foldLeft(getScala(g, paramNames)) {
        (acc, x) => acc + "\n" +  getScala(x, paramNames)
      }
    case f: FFunction =>
      "def %s(%s) {%s\n}".format(f.name,
        paramNames.foldLeft(""){ (acc, x) => acc + ", " + x + ": Int"}.drop(2),
        f.members.stmts.foldLeft("") { (acc, x) => x match {
          case FDeclaration(v) => acc
          case FAssignment(lhs, rhs) =>
            acc + "\n  " + "val %s = %s\t\t%s".format(getScala(lhs, paramNames), getScala(rhs, paramNames),
              f.varMap(lhs.name).toString)
        }})
    case FBasicBlock(stmts) =>
      stmts.foldLeft("") { (acc, x) => x match {
        case FDeclaration(v) => acc
        case FAssignment(lhs, rhs) => acc + "\n  " + getScala(x, paramNames)
      }}
    case FAssignment(lhs, rhs) =>
      "val %s = %s".format(getScala(lhs, paramNames), getScala(rhs, paramNames))
    case FDeclaration(variable) => ""
    case FLongConst(value) => value.toString + "l"
    case FVar(name) => name
    case FNeg(rhs) => "-(%s)".format(getScala(rhs, paramNames))
    case FAdd(lhs, rhs) => "(%s + %s)".format(getScala(lhs, paramNames), getScala(rhs, paramNames))
    case FSub(lhs, rhs) => "(%s - %s)".format(getScala(lhs, paramNames), getScala(rhs, paramNames))
    case FMult(lhs, rhs) => "(%s * %s)".format(getScala(lhs, paramNames), getScala(rhs, paramNames))
    case FDiv(lhs, rhs) => "(%s / %s)".format(getScala(lhs, paramNames), getScala(rhs, paramNames))
    case FRightShift(expr, bits) =>"(%s >> %d)".format(getScala(expr, paramNames), bits)
    case FLeftShift(expr, bits) =>"(%s << %d)".format(getScala(expr, paramNames), bits)
  }
}
