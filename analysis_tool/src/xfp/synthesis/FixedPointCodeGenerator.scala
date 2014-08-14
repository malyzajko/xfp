package xfp.synthesis

import xfp.trees._
import xfp.fixedpoint._
import xfp.utils.{BigRational => Rational}
import Rational._
import collection.mutable.Queue

//import collection.mutable.{HashMap => HMap, Map => MMap}
import xfp.fixedpoint.{FixedPointFormat => FPFormat}

object FixedPointCodeGenerator {

  var count = 0
  def treeToCode(tree: CExpr, stmts: Queue[CStmt],
    formats: collection.mutable.Map[String, FPFormat]): String = {
    tree match {
      case CNeg(rhs) =>
        val tmp = treeToCode(rhs, stmts, formats)
        val varName = "tmp"+count
        stmts += CAssignment(CVar(varName), CNeg(getLeaf(tmp)))
        formats += ((varName, tree.globalFormat))
        count += 1
        varName
      case CAdd(lhs, rhs) =>
        val tmp1 = treeToCode(lhs, stmts, formats)
        val tmp2 = treeToCode(rhs, stmts, formats)
        val varName = "tmp"+count
        stmts += CAssignment(CVar(varName), CAdd(getLeaf(tmp1), getLeaf(tmp2)))
        formats += ((varName, tree.globalFormat))
        count += 1
        varName
      case CSub(lhs, rhs) =>
        val tmp1 = treeToCode(lhs, stmts, formats)
        val tmp2 = treeToCode(rhs, stmts, formats)
        val varName = "tmp"+count
        stmts += CAssignment(CVar(varName), CSub(getLeaf(tmp1), getLeaf(tmp2)))
        formats += ((varName, tree.globalFormat))
        count += 1
        varName

      case CMult(CInv(y), x) => //CDiv(x, y)
        val tmpX = treeToCode(x, stmts, formats)
        val tmpY = treeToCode(y, stmts, formats)
        val varName = "tmp"+count
        stmts += CAssignment(CVar(varName), CDiv(getLeaf(tmpX), getLeaf(tmpY)))
        formats += ((varName, tree.globalFormat))
        count += 1
        varName

      case CMult(x, CInv(y)) =>
        val tmpX = treeToCode(x, stmts, formats)
        val tmpY = treeToCode(y, stmts, formats)
        val varName = "tmp"+count
        stmts += CAssignment(CVar(varName), CDiv(getLeaf(tmpX), getLeaf(tmpY)))
        formats += ((varName, tree.globalFormat))
        count += 1
        varName

      case CMult(lhs, rhs) =>
        val tmp1 = treeToCode(lhs, stmts, formats)
        val tmp2 = treeToCode(rhs, stmts, formats)
        val varName = "tmp"+count
        stmts += CAssignment(CVar(varName), CMult(getLeaf(tmp1), getLeaf(tmp2)))
        formats += ((varName, tree.globalFormat))
        count += 1
        varName

      case CInv(y) => // CDiv(1.0, y)
        val tmp = treeToCode(y, stmts, formats)
        val varName = "tmp"+count
        stmts += CAssignment(CVar(varName), CDiv(getLeaf("1.0"), getLeaf(tmp)))
        formats += ((varName, tree.globalFormat))
        count += 1
        varName

      case CDiv(lhs, rhs) =>
        val tmp1 = treeToCode(lhs, stmts, formats)
        val tmp2 = treeToCode(rhs, stmts, formats)
        val varName = "tmp"+count
        stmts += CAssignment(CVar(varName), CDiv(getLeaf(tmp1), getLeaf(tmp2)))
        formats += ((varName, tree.globalFormat))
        count += 1
        varName
      case CVar(name) =>
        formats += ((name, tree.globalFormat))
        name
      case CDoubleConst(v) => v.toString
    }
  }

  val Digit = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r

  def getLeaf(name: String): CExpr = {
    if (Digit.pattern.matcher(name).matches) CDoubleConst(name.toDouble)
    else CVar(name)
  }

  def translateToFP(tree: CTree, formats: Map[String, FPFormat]): FTree = tree match {
    case p: CProgram =>
      translateProgramToFP(p, formats)
    case b: CBasicBlock =>
      translateBlockToFP(b, formats)
    case _ =>
      println("Expected program or basic block, got " + tree.getClass())
      return null
  }


  def translateProgramToFP(program: CProgram, formats: Map[String, FPFormat]): FProgram = {
    var globalBB = translateBlockToFP(program.global, formats)
    var functions = List[FFunction]()
    for (fnc <- program.functions) {
      val fncFormats = fnc.varMap.mapValues((y: FixedForm) => y.format)
      val f = FFunction(fnc.name, translateBlockToFP(fnc.members, fncFormats))
      f.varMap ++= fncFormats
      functions :+= f
    }
    return FProgram(globalBB, functions)
  }

  def translateBlockToFP(block: CBasicBlock, formats: collection.Map[String, FPFormat]): FBasicBlock = {
    var statements = List[FStmt]()
    for (stmt <- block.stmts)
      stmt match {
        case CAssignment(l, r) =>
          statements :+= FAssignment(FVar(l.name), translateToFP(r, formats, formats(l.name)))
        case CDeclaration(v) =>
          statements :+= FDeclaration(FVar(v.name))
      }
    return FBasicBlock(statements)
  }

  // Only accept SSA format
  def translateToFP(expr: CExpr, formats: collection.Map[String, FixedPointFormat],
    resultFormat: FPFormat): FExpr = expr match {
    case CVar(name) => FVar(name)  // TODO: shift if output format does not match?
    case CDoubleConst(value) =>
      val bits = FPFormat.getFormat(Rational(value)).f
      FLongConst(doubleToLong(value, bits))
    case CNeg(CVar(name)) => FNeg(FVar(name))
    case CNeg(CDoubleConst(value)) =>
      val bits = FPFormat.getFormat(Rational(value)).f
      FNeg(FLongConst(doubleToLong(value, bits)))

    case CAdd(l: CLeaf, r: CLeaf) =>
      val mx = resultFormat.f
      val (ll, rr, mr) = alignOperators(l, r, formats)
      if (mx == mr) FAdd(ll, rr)
      else if (mx <= mr) FRightShift(FAdd(ll, rr), (mr - mx))
      else FLeftShift(FAdd(ll, rr), (mx - mr))  // Fixme: really?

    case CSub(l: CLeaf, r: CLeaf) =>
      val mx = resultFormat.f
      val (ll, rr, mr) = alignOperators(l, r, formats)
      if (mx == mr) FSub(ll, rr)
      else if (mx <= mr) FRightShift(FSub(ll, rr), (mr - mx))
      else FLeftShift(FSub(ll, rr), (mx - mr))  // Fixme: really?

    case CMult(l: CLeaf, r: CLeaf) =>
      val mx = resultFormat.f
      val (mult, mr) = multiplyOperators(l, r, formats)
      if (mx == mr) mult
      else if (mr - mx >= 0) FRightShift(mult, (mr - mx))
      else FLeftShift(mult, mx - mr)

    case CDiv(l: CLeaf, r: CLeaf) =>
      val mx = resultFormat.f
      divideOperators(l, r, mx, formats)

    case _ =>
      println("Expression not supported: " + expr.getClass)
      throw UnsupportedOperationException("Expression not supported: " + expr)
    // similar for rest
  }

  def alignOperators(x: CLeaf, y: CLeaf, formats: collection.Map[String, FixedPointFormat]):
    (FExpr, FExpr, Int) = (x, y) match {
    case (CVar(n1), CVar(n2)) =>
      val my = formats(n1).f
      val mz = formats(n2).f

      if (mz == my) (FVar(n1), FVar(n2), my)
      else if (my <= mz) (FLeftShift(FVar(n1), (mz - my)), FVar(n2), mz)
      else (FVar(n1), FLeftShift(FVar(n2), (my - mz)), my)

    case (CVar(n), CDoubleConst(v)) =>
      val my = formats(n).f
      val mz = FPFormat.getFormat(Rational(v)).f
      val longValue = doubleToLong(v, mz)
      if (my == mz) (FVar(n), FLongConst(longValue), mz)
      else if (my <= mz) (FLeftShift(FVar(n), (mz - my)), FLongConst(longValue), mz)
      else (FVar(n), FLeftShift(FLongConst(longValue), (my - mz)), my)

    case (CDoubleConst(v), CVar(n)) =>
      val mz = formats(n).f
      val my = FPFormat.getFormat(Rational(v)).f
      val longValue = doubleToLong(v, my)
      if (my == mz) (FLongConst(longValue), FVar(n), mz)
      else if (my <= mz) (FLeftShift(FLongConst(longValue), (mz - my)), FVar(n), mz)
      else (FLongConst(longValue), FLeftShift(FVar(n), (my - mz)), my)

    case (CDoubleConst(v1), CDoubleConst(v2)) =>
      val my = FPFormat.getFormat(Rational(v1)).f
      val mz = FPFormat.getFormat(Rational(v2)).f
      val i1 = doubleToLong(v1, my)
      val i2 = doubleToLong(v2, mz)
      if (my == mz) (FLongConst(i1), FLongConst(i2), mz)
      else if (my <= mz) (FLeftShift(FLongConst(i1), (mz - my)), FLongConst(i2), mz)
      else (FLongConst(i1), FLeftShift(FLongConst(i2), (my - mz)), my)
  }

  def multiplyOperators(x: CLeaf, y: CLeaf, formats: collection.Map[String, FixedPointFormat]): (FMult, Int) = (x, y) match {
     case (CVar(n1), CVar(n2)) =>
      val my = formats(n1).f
      val mz = formats(n2).f
      (FMult(FVar(n1), FVar(n2)), my + mz)

    case (CVar(n), CDoubleConst(v)) =>
      val my = formats(n).f
      val mz = FPFormat.getFormat(Rational(v)).f
      val i = doubleToLong(v, mz)
      (FMult(FVar(n), FLongConst(i)), my + mz)

    case (CDoubleConst(v), CVar(n)) =>
      val my = FPFormat.getFormat(Rational(v)).f
      val i = doubleToLong(v, my)
      val mz = formats(n).f
      (FMult(FLongConst(i), FVar(n)), my + mz)

    case (CDoubleConst(v1), CDoubleConst(v2)) =>
      val my = FPFormat.getFormat(Rational(v1)).f
      val i1 = doubleToLong(v1, my)
      val mz = FPFormat.getFormat(Rational(v2)).f
      val i2 = doubleToLong(v2, mz)
      (FMult(FLongConst(i1), FLongConst(i2)), my + mz)
   }

   def divideOperators(x: CLeaf, y: CLeaf, mx: Int, formats: collection.Map[String, FixedPointFormat]): FDiv = (x, y) match {
     case (CVar(n1), CVar(n2)) =>
      val my = formats(n1).f
      val mz = formats(n2).f
      val shift = mx + mz - my
      FDiv(FLeftShift(FVar(n1), shift), FVar(n2))

    case (CVar(n), CDoubleConst(v)) =>
      val my = formats(n).f
      val mz = FPFormat.getFormat(Rational(v)).f
      val i = doubleToLong(v, mz)
      val shift = mx + mz - my
      FDiv(FLeftShift(FVar(n), shift), FLongConst(i))

    case (CDoubleConst(v), CVar(n)) =>
      val my = FPFormat.getFormat(Rational(v)).f
      val i = doubleToLong(v, my)
      val mz = formats(n).f
      val shift = mx + mz - my
      FDiv(FLeftShift(FLongConst(i), shift), FVar(n))

    case (CDoubleConst(v1), CDoubleConst(v2)) =>
      val my = FPFormat.getFormat(Rational(v1)).f
      val i1 = doubleToLong(v1, my)
      val mz = FPFormat.getFormat(Rational(v2)).f
      val i2 = doubleToLong(v2, mz)
      val shift = mx + mz - my
      FDiv(FLeftShift(FLongConst(i1), shift), FLongConst(i2))
   }


  private def doubleToLong(d: Double, f: Int): Long = {
    return (d * math.pow(2, f)).round.toLong
  }

}
