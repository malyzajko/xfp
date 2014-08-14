package xfp.analysis

import xfp.trees._
import xfp.fixedpoint._
import scala.util.control.Breaks._

import collection.mutable.{HashMap, Map => MMap}
import xfp.fixedpoint.{FixedPointFormat => FPFormat}
import xfp.utils.{BigRational => Rational}

object FixedPointAnalyzer {
  import FixedForm._

  var log: String = ""

  // Modifies the tree to add the analysis results to each function
  def analyzeAll(tree: CTree, inputs: Map[String, (FPFormat, Interval)],
    formats: Map[String, FixedPointFormat], bits: Int = 16, rounding: Boolean = false): Boolean = {
    tree match {
      case program: CProgram =>
        // Setting up defaults
        FixedPointFormat.globalBitLength = bits
        FixedPointFormat.globalRounding = rounding
        if (formats.size != 0) xfp.fixedpoint.checkForOverflow = false

        val globalVariableMap = new HashMap[String, FixedForm]()
        processBasicBlock(program.global, globalVariableMap, formats)
        if (log != "") { println(log); return false}
        for ((key, (format, interval)) <- inputs)
          globalVariableMap += ((key, FixedForm(interval, format)))

        for (fnc <- program.functions) {
          fnc.varMap.clear    //reset previous analyses
          fnc.varMap ++= globalVariableMap
          processBasicBlock(fnc.members, fnc.varMap, formats)
        }
        if (log != "") { println(log); return false}
        else return true
      case _ =>
        println("Wrong input: program expected, found " + tree.getClass())
        return false
    }
  }

  def processBasicBlock(basicBlock: CBasicBlock, varMap: MMap[String, FixedForm],
    formats: Map[String, FixedPointFormat]): Unit = {
    val formatsAvailable = formats.size > 0
    for (stmt <- basicBlock.stmts) {
      stmt match {
        case CDeclaration(CVar(name)) => ;
        case CAssignment(CVar(name), rhs) =>
          try {
            if (formatsAvailable)
              varMap += ((name, evalSSA(rhs, varMap, formats(name))))
            else  // Note: if we don't have formats, we simply overwrite the format in the variable map
              varMap += ((name, eval(rhs, varMap)))
          }
          catch {
            case FixedPointOverflowException(s) =>
              log += "Cannot process assignment: %s\n".format(stmt.toString)
              return
            case e: java.util.NoSuchElementException =>
              log += "Missing input parameter info: %s\n".format(e.getMessage)
              return
          }
      }
    }
  }

  def analyzeExpr(expr: CExpr, inputs: Map[String, (FPFormat, Interval)], bits: Int,
    rounding: Boolean): FixedForm = {
    FixedPointFormat.globalBitLength = bits
    FixedPointFormat.globalRounding = rounding
    val globalVariableMap = new HashMap[String, FixedForm]()
    for ((key, (format, interval)) <- inputs)
      globalVariableMap += ((key, FixedForm(interval, format)))
    eval(expr, globalVariableMap)
  }

  // Java interface version...
  def analyzeExprJava(expr: CExpr, inputFormats: java.util.Map[String, FPFormat],
    inputRanges: java.util.Map[String, Interval], bits: Int, rounding: Boolean): FixedForm = {
    FixedPointFormat.globalBitLength = bits
    FixedPointFormat.globalRounding = rounding
    val globalVariableMap = new HashMap[String, FixedForm]()
    val iter = inputFormats.entrySet.iterator
    while (iter.hasNext) {
      val entry = iter.next
      globalVariableMap += ((entry.getKey, FixedForm(inputRanges.get(entry.getKey), entry.getValue)))
    }
    try {
      eval(expr, globalVariableMap)
    }
    catch {
      case FixedPointOverflowException(s) =>
        // Something hopefully sufficiently large
        return new FixedForm(Rational(0.0), Rational(10000.0))
    }
  }

  def eval(expr: CExpr, varMap: collection.mutable.Map[String, FixedForm]): FixedForm = {
    val tmp = expr match {
      case CVar(name) => varMap(name)
      case CDoubleConst(value) => new FixedForm(Rational(value))
      case CNeg(rhs) => - eval(rhs, varMap)
      case CAdd(lhs, rhs) => eval(lhs, varMap) + eval(rhs, varMap)
      case CSub(lhs, rhs) => eval(lhs, varMap) - eval(rhs, varMap)
      case CMult(lhs, rhs) => eval(lhs, varMap) * eval(rhs, varMap)
      case CDiv(lhs, rhs) => eval(lhs, varMap) / eval(rhs, varMap)
      case CInv(expr) => eval(expr, varMap).inverse()
    }
    expr.globalFormat = tmp.format
    //println(expr + " has format " + expr.globalFormat)
    tmp
  }


  // TODO: deal with assigments like x = y with different formats
  def evalSSA(expr: CExpr, varMap: collection.mutable.Map[String, FixedForm],
    format: FixedPointFormat): FixedForm = expr match {
    case CVar(name) => varMap(name)
    case CDoubleConst(value) => new FixedForm(Rational(value))
    case CNeg(rhs) => - eval(rhs, varMap)
    case CAdd(lhs, rhs) => eval(lhs, varMap).plus(eval(rhs, varMap), format)
    case CSub(lhs, rhs) => eval(lhs, varMap).minus(eval(rhs, varMap), format)
    case CMult(lhs, rhs) => eval(lhs, varMap).times(eval(rhs, varMap), format)
    case CDiv(lhs, rhs) => eval(lhs, varMap).divide(eval(rhs, varMap), format)
    case CInv(expr) => eval(expr, varMap).inverse(format)
  }


  /*
    ------------------ Interval subdivision -------------------
  */
  var subdivs = 0

  /**
    Uses a recursive subdivision strategy to find a better error estimate, if possible.
    In order for this to be sounds, we may only subdivide on one single variable at a time.
  */
  def analyzeExprStrategy1(expr: CExpr, inputs: Map[String, (FPFormat, Interval)],
    bits: Int, rounding: Boolean): Double = {
    // Setting up defaults
    FixedPointFormat.globalBitLength = bits
    FixedPointFormat.globalRounding = rounding
    val globalVariableMap = new HashMap[String, FixedForm]()
    for ((key, (format, interval)) <- inputs)
      globalVariableMap += ((key, FixedForm(interval, format)))
    val initialResult = eval(expr, globalVariableMap)
    println("initial result: " + initialResult)
    subdivs = 0
    val s = subdivide(expr, globalVariableMap, inputs, initialResult.maxAbsError, 1, Rational(0))
    println("FINAL subdivision result: " + s)
    println("subdivisions performed: " + subdivs)
    return s
  }

  // Subdivide on each variable, and return the minimum maximum error
  def subdivide(expr: CExpr, varMap: collection.mutable.Map[String, FixedForm],
    inputs: Map[String, (FPFormat, Interval)], initialMaxError: Rational, count: Int, minError: Rational): Double = {
    var errors = List[Double]()
    for ((key, (f, i)) <- inputs) {
      errors :+= subdivide(expr, key, varMap, inputs, initialMaxError, count, minError).toDouble
    }
    //println("errors: " + errors)
    return errors.min
  }

  def subdivide(expr: CExpr, key: String, varMap: collection.mutable.Map[String, FixedForm],
    inputs: Map[String, (FPFormat, Interval)], maxError: Rational, count: Int, minError: Rational): Rational = {
    val format = inputs(key)._1
    val i = inputs(key)._2
    subdivs += 1
    val variableMap1 = varMap + ((key, FixedForm(Interval(i.xlo, i.xlo/2 + i.xhi/2), format))) // Fixme: Not fully sound
    val variableMap2 = varMap + ((key, FixedForm(Interval(i.xlo/2 + i.xhi/2, i.xhi), format)))
    val res1 = eval(expr, variableMap1)
    val res2 = eval(expr, variableMap2)
    val newMaxError = Rational.max(res1.maxAbsError, res2.maxAbsError)
    // Do not continue subdividing if we can't improve the result anyway
    if (newMaxError < maxError && count < 5 && newMaxError > minError) {
      val e1 = subdivide(expr, key, variableMap1, inputs + ((key, (format, Interval(i.xlo, i.xlo/2 + i.xhi/2)))),
                         res1.maxAbsError, count + 1, Rational(0))
      val e2 = subdivide(expr, key, variableMap2, inputs + ((key, (format, Interval(i.xlo/2 + i.xhi/2, i.xhi)))),
                         res2.maxAbsError, count + 1, e1)
      return Rational.max(e1, e2)
    }
    else if (newMaxError < maxError) {
      return newMaxError
    }
    return maxError
  }
}
