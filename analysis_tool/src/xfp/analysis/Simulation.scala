package xfp.analysis

import xfp.trees._
import xfp.synthesis.FixedPointCodeGenerator
import xfp.fixedpoint.{Interval, FixedPointFormat}
import collection.mutable.{HashMap => HMap, Map => MMap}
import collection.mutable.Queue
import scala.io.Source

object Simulation {

  /**
   * @return maximum seen absolute error
   */
  def simulate(ctree: CExpr, ftree: List[FStmt], inputs: Map[String,(FixedPointFormat, Interval)],
    maxTries: Int, seed: Long = System.currentTimeMillis): Double = {
    val r = new scala.util.Random(seed)
    val outputFrac = ctree.globalFormat.f
    if (xfp.fixedpoint.debugMode) println("outputFrac: " + outputFrac)
    var maxError = 0.0
    var i = 0
    while (i < maxTries) {
      val inputsDbl = new collection.mutable.HashMap[String, Double]
      val inputsFp = new collection.mutable.HashMap[String, Long]
      for ((key, (format, range)) <- inputs) {
        val x = range.xlo + r.nextDouble * (range.xhi - range.xlo)
        val l = doubleToLong(x, format.f)
        inputsDbl += ((key, x))
        inputsFp += ((key, l))
      }
      val doubleResult = evalDouble(ctree, inputsDbl)
      var error = 1000000.0
      try {
        val fixedptResult = evalFp(ftree, inputsFp)
        error = math.abs(doubleResult - longToDouble(fixedptResult, outputFrac))
      } catch {
        case e: Exception => ;
      }
      maxError = math.max(maxError, error)
      i += 1
    }
    //println("maxError = " + printFloat(maxError))
    return maxError
  }

  def doubleToLong(d: Double, f: Int): Long = {
    return (d * math.pow(2, f)).round.toLong
  }
  def longToDouble(i: Long, f: Int): Double = {
    return i.toDouble / math.pow(2, f)
  }

  def evalDouble(expr: CExpr, varMap: collection.mutable.Map[String, Double]): Double = expr match {
      case CVar(name) => varMap(name)
      case CDoubleConst(value) => value
      case CNeg(rhs) => - evalDouble(rhs, varMap)
      case CAdd(lhs, rhs) => evalDouble(lhs, varMap) + evalDouble(rhs, varMap)
      case CSub(lhs, rhs) => evalDouble(lhs, varMap) - evalDouble(rhs, varMap)
      case CMult(lhs, rhs) => evalDouble(lhs, varMap) * evalDouble(rhs, varMap)
      case CDiv(lhs, rhs) => evalDouble(lhs, varMap) / evalDouble(rhs, varMap)
      case CInv(expr) => 1.0 / evalDouble(expr, varMap)
  }

  def evalFp(ftree: List[FStmt], varMap: collection.mutable.Map[String, Long]): Long = {
    for (stmt <- ftree) {
      stmt match {
        case FAssignment(FVar(name), rhs) =>
          val result = evalLong(rhs, varMap)
          varMap += ((name, result))
        case _=> assert(false)
       }
    }
    return varMap(ftree.last.asInstanceOf[FAssignment].lhs.name)
  }

  def evalLong(expr: FExpr, varMap: collection.mutable.Map[String, Long]): Long = expr match {
      case FVar(name) => varMap(name)
      case FLongConst(value) => value
      case FNeg(rhs) => - evalLong(rhs, varMap)
      case FAdd(lhs, rhs) => evalLong(lhs, varMap) + evalLong(rhs, varMap)
      case FSub(lhs, rhs) => evalLong(lhs, varMap) - evalLong(rhs, varMap)
      case FMult(lhs, rhs) => evalLong(lhs, varMap) * evalLong(rhs, varMap)
      case FDiv(lhs, rhs) => evalLong(lhs, varMap) / evalLong(rhs, varMap)
      case FRightShift(e, bits) => evalLong(e, varMap) >> bits
      case FLeftShift(e, bits) => evalLong(e, varMap) << bits
  }





}
