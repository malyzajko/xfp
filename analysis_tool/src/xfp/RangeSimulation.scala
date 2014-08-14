package xfp.analysis

import xfp.trees._

import collection.mutable.Queue
import scala.io.Source

import xfp.fixedpoint._
import xfp.synthesis.FixedPointCodeGenerator
import xfp.utils.{BigRational => Rational}
import Rational._


object RangeSimulation {
  var r = new scala.util.Random(System.currentTimeMillis)

  var numTries = 0

  /**
    * Takes as input a file with expressions (on separate lines), a file with the
    * input ranges (in Simulink format) and the number of random values to try.
    * The input ranges apply to all expressions.
    * For each expressions the ranges of all intermediate values are computed with
    * simulation. From this, fixedpoint code is generated and its precision wrt to
    * the floating-point version is determined again via simulation.
    * The number of tries in each simulation is the same (third argument).
    */
  def main(args: Array[String]) {
    if (args.size != 3 && args.size != 4) {
      println("Usage: RangeSimulation expressionFile inputsFile noTries sameSeed?")
      return
    }
    // inputs: Map[String,(FixedPointFormat, Interval)]
    val (inputs, formats) = InputVariablesParser.parseFile(args(1))
    var inputRanges = new collection.immutable.HashMap[String, Interval]()
    for ((k, v) <- inputs) inputRanges += ((k, v._2))

    numTries = args(2).toInt

    val seed = if (args.size == 4) System.currentTimeMillis else 0

    // Set correct overall bitlength for this exercise
    FixedPointFormat.globalBitLength = inputs.head._2._1.bits

    var counter = 0
    for (line <- Source.fromFile(args(0)).getLines()) {
      if (! (line.length == 0 || line.startsWith("//"))) {  //ignore empty lines and comments
        val expr = ExprParser.parse(line).asInstanceOf[CExpr]
        if (expr == null) { println("Parse error. " + line); return }
        print(counter + ": ")

        val error =
          if (seed == 0) run(expr, inputRanges, inputs)
          else run(expr, inputRanges, inputs, seed)  //use same seed each time
        println("maxError: " + xfp.utils.printFloat(error))
        counter += 1
      }
    }

  }

  // Java interface version for GP tool
  def simulateExprJava(expr: CExpr, inputFormats: java.util.Map[String, FixedPointFormat],
    inputRanges: java.util.Map[String, Interval], bits: Int, trials: Int): Double = {
    FixedPointFormat.globalBitLength = bits
    numTries = trials

    var inputs = new collection.immutable.HashMap[String, (FixedPointFormat, Interval)]()
    var inputRangesScala = new collection.immutable.HashMap[String, Interval]()
    val iter = inputFormats.entrySet.iterator
    while (iter.hasNext) {
      val entry = iter.next
      val range = inputRanges.get(entry.getKey)
      inputs += ((entry.getKey, (entry.getValue, range)))
      inputRangesScala += ((entry.getKey, range))
    }
    // different seed each time
    return run(expr, inputRangesScala, inputs, print=false)
  }

  // default is to take new seed each time
  private def run(expr: CExpr, inputRanges: Map[String, Interval],
    inputs: Map[String,(FixedPointFormat, Interval)], seed: Long = System.currentTimeMillis,
    print: Boolean = false): Double = {
    // Remember the seed used so that we can do the error simulation
    // with exactly the same inputs (so that we're not out of range).
    //val seed = System.currentTimeMillis
    //println("using seed " + seed)
    r = new scala.util.Random(seed)

    var counter = 0
    while(counter < numTries) {
      var randomInput = new collection.immutable.HashMap[String, Double]()
      for ((k, v) <- inputRanges) randomInput += ((k, v.xlo + r.nextDouble * (v.xhi - v.xlo)))

      simulate(expr, randomInput)
      counter += 1
    }
    //simulateInterval(expr, inputRanges)

    // Try the endpoints of the intervals as well.
    val boundaries = boundaryInputs(inputRanges)
    for (b <- boundaries) simulate(expr, b)

    // translate globalRange to globalFormat
    assignFormats(expr)

    // generate code
    val stmts = new Queue[CStmt]()
    val map = new collection.mutable.HashMap[String, FixedPointFormat]()
    FixedPointCodeGenerator.count = 0
    FixedPointCodeGenerator.treeToCode(expr, stmts, map)
    val ftree = FixedPointCodeGenerator.translateToFP(CBasicBlock(stmts.toList), map.toMap)

    // Uncomment this, if you want to see the code
    val paramNames: Iterable[String] = inputs.keys
    if (print) println(Printer.getScala(ftree, paramNames))

    // Do the simulation...
    return xfp.analysis.Simulation.simulate(expr, ftree.asInstanceOf[FBasicBlock].stmts, inputs, numTries, seed)
  }

  private def boundaryInputs(map: Map[String, Interval]): List[Map[String, Double]] = {
    var newMaps: List[Map[String, Double]] = List.empty
    val (key, intrvl) = map.head
    if (map.size == 1) {
      newMaps :+= Map(key -> intrvl.xlo)
      newMaps :+= Map(key -> intrvl.xhi)
      if (intrvl.xlo <= 0.0 && intrvl.xhi >= 0.0) {
        newMaps :+= Map(key -> 0.0)
      }
    }
    else {
      val remaining = boundaryInputs(map - key)
      for (m <- remaining) {
        newMaps :+= m + ((key, intrvl.xlo))
        newMaps :+= m + ((key, intrvl.xhi))
        if (intrvl.xlo <= 0.0 && intrvl.xhi >= 0.0) {
          newMaps :+= m + ((key, 0.0))
        }
      }
    }
    return newMaps
  }

  /**
    * Evaluates expression expr with the given inputs and overrides
    * the globalRange if the range at any intermediate value is larger.
    */
  private def simulate(expr: CExpr, map: Map[String, Double]): Double = {
    val exprValue = expr match {
      case CVar(name) => map(name)
      case CDoubleConst(value) => value
      case CNeg(rhs) => - simulate(rhs, map)
      case CAdd(lhs, rhs) => simulate(lhs, map) + simulate(rhs, map)
      case CSub(lhs, rhs) => simulate(lhs, map) - simulate(rhs, map)
      case CMult(lhs, rhs) => simulate(lhs, map) * simulate(rhs, map)
      case CDiv(lhs, rhs) => simulate(lhs, map) / simulate(rhs, map)
      case CInv(expr) => 1.0 / simulate(expr, map)
    }
    val rd = Rational(exprValue)
    if (expr.globalRange == null) {
      expr.globalRange = RationalInterval(rd, rd)
    }
    else {
      // Track the maximizing assignments
      if (expr.globalRange.xlo > rd) expr.min = map.toString
      if (expr.globalRange.xhi < rd) expr.max = map.toString
      expr.globalRange =
        RationalInterval(min(expr.globalRange.xlo, rd), max(expr.globalRange.xhi, rd))
    }
    exprValue
  }

  /**
    * Evaluates expression expr with the given inputs and overrides
    * the globalRange if the range at any intermediate value is larger.
    */
  /*private def simulateInterval(expr: CExpr, map: Map[String, Interval]): RationalInterval  = {
    val exprValue = expr match {
      case CVar(name) => new RationalInterval(map(name))
      case CDoubleConst(value) => RationalInterval(Rational(value), Rational(value))
      case CNeg(rhs) => - simulateInterval(rhs, map)
      case CAdd(lhs, rhs) => simulateInterval(lhs, map) + simulateInterval(rhs, map)
      case CSub(lhs, rhs) => simulateInterval(lhs, map) - simulateInterval(rhs, map)
      case CMult(lhs, rhs) => simulateInterval(lhs, map) * simulateInterval(rhs, map)
      case CDiv(lhs, rhs) => simulateInterval(lhs, map) / simulateInterval(rhs, map)
      case CInv(expr) => RationalInterval(Rational(1.0), Rational(1.0)) / simulateInterval(expr, map)
    }
    expr.globalRange = exprValue
    exprValue
  }*/



  private def assignFormats(expr: CExpr): Unit = {
    try {
      expr.globalFormat = FixedPointFormat.getFormat(expr.globalRange.xlo, expr.globalRange.xhi)
    }
    catch {
      case FixedPointOverflowException(msg) =>
        println("FixedPointOveflowException at expression: " + expr)
        println("Range " + expr.globalRange + " does not fit bitlength " + FixedPointFormat.globalBitLength)
        println("Minimizing assignment: " + expr.min)
        println("Maximizing assignment: " + expr.max)
        System.exit(1)
    }
    expr match {
      case CNeg(rhs) => assignFormats(rhs)
      case CAdd(lhs, rhs) => assignFormats(lhs); assignFormats(rhs)
      case CSub(lhs, rhs) => assignFormats(lhs); assignFormats(rhs)
      case CMult(lhs, rhs) => assignFormats(lhs); assignFormats(rhs)
      case CDiv(lhs, rhs) => assignFormats(lhs); assignFormats(rhs)
      case CInv(expr) => assignFormats(expr)
      case _ => ;
    }
  }

}
