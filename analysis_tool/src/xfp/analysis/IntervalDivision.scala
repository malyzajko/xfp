package xfp.analysis

import xfp.fixedpoint._
import xfp.trees._
import xfp.utils.{BigRational => Rational}
import Rational._
import collection.immutable.HashMap
import collection.mutable.LinkedList
import RationalForm._

object IntervalDivision {

  /*
    Computes the maximum error by subdividing the input ranges systematically.
    Note that the result for using one subdivision (i.e. not subdividing at all)
    will be the same as using FixedForms if we have operations {+, -, *} only.
    If we have division, there may be slight differences. This is due to the fact
    that in this procedure, we compute the ranges of rational numbers first, without
    taking into account error in order to compute the formats that we need.
  */
  def computeError(inputs: Map[String, (FixedPointFormat, Interval)], problem: CExpr,
    inputsToDivideOn: List[String], numSteps: Int): Rational  = {
    //val numSteps = 5
    // Preprocess input
    var inputFormats = new HashMap[String, FixedPointFormat]()
    var fullInputForms = new HashMap[String, FixedForm]()
    for ((key, value) <- inputs) {
      inputFormats += ((key, value._1))
      fullInputForms += ((key, FixedForm(value._2, value._1)))
    }

    // Generate subdivisions
    var inputForms = new LinkedList[Map[String, FixedForm]]()
    // Strategy 1: in turn subdivide on one variable and keep the others fixed
    for ((key, format) <- inputFormats) {
      if (inputsToDivideOn.contains(key)) {
        val interval = inputs(key)._2
        val stepSize = (Rational(interval.xhi) - Rational(interval.xlo)) / Rational(numSteps)
        val noise = stepSize / Rational(2.0)
        val xlo = Rational(interval.xlo)
        for (i <- 0 until numSteps) {
          inputForms :+= (fullInputForms + ((key, new FixedForm(format, xlo + Rational(i) * stepSize + noise, noise))))
          //inputForms :+= fullInputForms
        }
      }
    }
    //for (i <- 0 until inputForms.length) { println("input \t\t" + inputForms(i)) }

    reset(problem, inputForms.length) // make space for certain amount of cached info

    // Get local ranges for intermediate values
    for (i <- 0 until inputForms.length) {
      computeLocalRange(problem, i, inputForms(i))  // we need the index for caching
    }

    // Compute global ranges and from them formats
    computeGlobalRange(problem, inputFormats)

    // Compute local errors with global formats
    xfp.fixedpoint.checkForOverflow = false
    var maxError = Rational(0)
    for (i <- 0 until inputForms.length) {
      val localError = computeGlobalError(problem, inputForms(i))
      maxError = max(maxError, localError.maxAbsError)
    }
    return maxError
  }

/*  def generateSubdivision(inputs: Map[String, (FixedPointFormat, Interval)], inputsToDivideOn: List[String]) = {
    var divisions = new HashMap[String, List[FixedForm]]()  //mapping variable -> interval divisions

    for (variable <- inputsToDivideOn) {
      val interval = inputs(variable)._2
      val stepSize = (Rational(interval.xhi) - Rational(interval.xlo)) / Rational(numSteps)
      val noise = stepSize / Rational(2.0)
      val xlo = Rational(interval.xlo)
      val list = new LinkedList[FixedForm]()
      val format = inputs(variable)._1
      for (i <- 0 until numSteps) {
        list :+= new FixedForm(format, xlo + Rational(i) * stepSize + noise, noise)
      }
      divisions += ((variable, list))
    }
  }*/

  private def computeLocalRange(expr: CExpr, index: Int, inputs: Map[String, FixedForm]): FixedForm = {
    val tmp = expr match {
      case CVar(name) => inputs(name)
      case CDoubleConst(value) => new FixedForm(Rational(value))
      case CNeg(rhs) => - computeLocalRange(rhs, index, inputs)
      case CAdd(lhs, rhs) => computeLocalRange(lhs, index, inputs) + computeLocalRange(rhs, index, inputs)
      case CSub(lhs, rhs) => computeLocalRange(lhs, index, inputs) - computeLocalRange(rhs, index, inputs)
      case CMult(lhs, rhs) => computeLocalRange(lhs, index, inputs) * computeLocalRange(rhs, index, inputs)
      case inv: CInv => computeLocalRange(inv.expr, index, inputs).inverse()
      case CDiv(lhs, rhs) => throw UnsupportedOperationException("Use CMult(x, CInv(y)) instead!"); return null
    }
    expr.cache(index) = new RationalInterval(tmp)
    tmp
  }

  private def computeGlobalRange(expr: CExpr, inputFormats: Map[String, FixedPointFormat]): Unit = {
    expr match {
      case CVar(name) => expr.globalFormat = inputFormats(name) //overwrite by user input
      case CDoubleConst(value) => expr.globalFormat = FixedPointFormat.getFormat(Rational(value));
      case rest =>
        expr.globalRange = expr.cache.foldLeft(expr.cache(0))((x: RationalInterval, y: RationalInterval) => x union y )
        expr.globalFormat = FixedPointFormat.getFormat(expr.globalRange.xlo, expr.globalRange.xhi)
        rest match {
          case CNeg(rhs) => computeGlobalRange(rhs, inputFormats)
          case CAdd(lhs, rhs) => computeGlobalRange(lhs, inputFormats); computeGlobalRange(rhs, inputFormats)
          case CSub(lhs, rhs) => computeGlobalRange(lhs, inputFormats); computeGlobalRange(rhs, inputFormats)
          case CMult(lhs, rhs) => computeGlobalRange(lhs, inputFormats); computeGlobalRange(rhs, inputFormats)
          case CInv(rhs) => computeGlobalRange(rhs, inputFormats)
          case CDiv(lhs, rhs) => throw UnsupportedOperationException("Use CMult(x, CInv(y)) instead!")
          case _ => ; // covered above
        }
    }
  }

  private def computeGlobalError(expr: CExpr, inputs: Map[String, FixedForm]): FixedForm = {
    expr match {
      case CVar(name) => inputs(name)

      case CDoubleConst(value) => new FixedForm(Rational(value))

      case CNeg(rhs) => - computeGlobalError(rhs, inputs)

      case CAdd(lhs, rhs) =>
        computeGlobalError(lhs, inputs).plus(computeGlobalError(rhs, inputs), expr.globalFormat)

      case CSub(lhs, rhs) =>
        computeGlobalError(lhs, inputs).minus(computeGlobalError(rhs, inputs), expr.globalFormat)

      case CMult(lhs, rhs) =>
        computeGlobalError(lhs, inputs).times(computeGlobalError(rhs, inputs), expr.globalFormat)

      case inv: CInv =>
        computeGlobalError(inv.expr, inputs).inverse(expr.globalFormat)

      case CDiv(lhs, rhs) => throw UnsupportedOperationException("Use CMult(x, CInv(y)) instead!"); return null;
    }

  }

  private def reset(tree: CExpr, cacheSize: Int): Unit = {
    tree.cache = new Array[RationalInterval](cacheSize)
    tree match {
      case CDoubleConst(v) => ;
      case CVar(name) => ;
      case CNeg(rhs) => reset(rhs, cacheSize)
      case CAdd(lhs, rhs) => reset(lhs, cacheSize); reset(rhs, cacheSize);
      case CSub(lhs, rhs) => reset(lhs, cacheSize); reset(rhs, cacheSize);
      case CMult(lhs, rhs) => reset(lhs, cacheSize); reset(rhs, cacheSize);
      case inv: CInv => reset(inv.expr, cacheSize)
      case CDiv(lhs, rhs) => throw UnsupportedOperationException("Use CMult(x, CInv(y)) instead!")
    }
  }
}
