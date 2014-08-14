package xfp.trees

import xfp.fixedpoint.FixedPointFormat
import collection.mutable.HashMap

sealed abstract class FTree

case class FProgram(global: FBasicBlock, functions: List[FFunction]) extends FTree
case class FFunction(name: String, members: FBasicBlock) extends FTree {
  val varMap = new HashMap[String, FixedPointFormat]()
}

case class FBasicBlock(stmts: List[FStmt]) extends FTree

abstract class FStmt extends FTree
case class FAssignment(lhs: FVar, rhs: FExpr) extends FStmt
case class FDeclaration(variable: FVar) extends FStmt


/*
  This is now fixed-point specific. Fixedpoint expressions consist of
  addition, subtraction, multiplication and left and right shift.
  All values are integers.
 */
sealed abstract class FExpr extends FTree

// Make long default since otherwise we get overflows
case class FLongConst(value: Long) extends FExpr {
  override def toString = "c_" + value.toString
}


case class FVar(name: String) extends FExpr {
  override def toString = name
}

case class FNeg(rhs: FExpr) extends FExpr {
  override def toString = "(-" + rhs + ")"
}

case class FAdd(lhs: FExpr, rhs: FExpr) extends FExpr {
  override def toString = "(" + lhs + " + " + rhs + ")"
}

case class FSub(lhs: FExpr, rhs: FExpr) extends FExpr {
  override def toString = "(" + lhs + " - " + rhs + ")"
}

case class FMult(lhs: FExpr, rhs: FExpr) extends FExpr {
  override def toString = "(" + lhs + " * " + rhs + ")"
}

case class FDiv(lhs: FExpr, rhs: FExpr) extends FExpr {
  override def toString = "(" + lhs + " / " + rhs + ")"
}


case class FRightShift(expr: FExpr, bits: Int) extends FExpr {
  override def toString = "(" + expr + " >> " + bits + ")"
}

case class FLeftShift(expr: FExpr, bits: Int) extends FExpr {
  override def toString = "(" + expr + " << " + bits + ")"
}
