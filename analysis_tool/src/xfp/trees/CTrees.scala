package xfp.trees

import xfp.fixedpoint.{FixedForm, RationalInterval, FixedPointFormat}
import collection.mutable.HashMap
import xfp.utils.{BigRational => Rational}


sealed abstract class CTree

case class CProgram(global: CBasicBlock, functions: List[CFunction]) extends CTree

case class CFunction(name: String, members: CBasicBlock) extends CTree {
  val varMap = new HashMap[String, FixedForm]()
}

case class CBasicBlock(stmts: List[CStmt]) extends CTree


abstract class CStmt extends CTree
case class CAssignment(lhs: CVar, rhs: CExpr) extends CStmt
case class CDeclaration(variable: CVar) extends CStmt

sealed abstract class CExpr extends CTree {
  var cache: Array[RationalInterval] = null
  var globalRange: RationalInterval = null
  var globalFormat: FixedPointFormat = null
  // For range simulation
  var min: String = null
  var max: String = null
  // for expression enumeration
  var done: Boolean = false
}
sealed abstract class CLeaf extends CExpr

case class CDoubleConst(value: Double) extends CLeaf {
  override def toString = value.toString
}

case class CVar(name: String) extends CLeaf {
  override def toString = name
}

case class CNeg(rhs: CExpr) extends CExpr {
  override def toString = "-" + rhs
}

case class CAdd(lhs: CExpr, rhs: CExpr) extends CExpr {
  override def toString = "(" + lhs + " + " + rhs + ")"
}

case class CSub(lhs: CExpr, rhs: CExpr) extends CExpr {
  override def toString = "(" + lhs + " - " + rhs + ")"
}

case class CMult(lhs: CExpr, rhs: CExpr) extends CExpr {
  override def toString = "(" + lhs + " * " + rhs + ")"
}

case class CDiv(lhs: CExpr, rhs: CExpr) extends CExpr {
  override def toString = "(" + lhs + " / " + rhs + ")"
}

case class CInv(expr: CExpr) extends CExpr {
  override def toString = "1/(" + expr + ")"
}
