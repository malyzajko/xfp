
import xfp.analysis._
import xfp.trees._
import collection.immutable.HashMap
import xfp.fixedpoint.{Interval, FixedPointFormat => FPF}
import collection.mutable.Queue
import xfp.synthesis._

object TestMain {
  def useIntervalSubdivision(problems: Array[String], inputs: HashMap[String, (FPF, Interval)],
    divideOn: List[String]) {
    for (i <- 0 until problems.length) {
      print("Computing for problem " + i)
      val ctree = ExprParser.parse(problems(i))
      //println(ctree)
      val maxError = IntervalDivision.computeError(inputs, ctree, divideOn, 1)
      println("  total error " + i + ": " + "%1.4e".format(maxError.toDouble))

      /*val stmts = new Queue[CStmt]()
      val map = new collection.mutable.HashMap[String, FPF]()
      FixedPointCodeGenerator.count = 0
      FixedPointCodeGenerator.treeToCode(ctree, stmts, map)
      println(stmts)
      println(map)
      val ftree = FixedPointCodeGenerator.translateToFP(CBasicBlock(stmts.toList), map.toMap)
      val paramNames: Iterable[String] = log_poly_inputs.keys
      println(Printer.getScala(ftree, paramNames))
      */
    }
  }

  def useStandard(problems: Array[String], inputs: HashMap[String, (FPF, Interval)]) {
    for (i <- 0 until problems.length) {
      println("Computing for problem " + i)
      val ctree = ExprParser.parse(problems(i))
      val maxError = FixedPointAnalyzer.analyzeExpr(ctree, inputs, 16, false)
      println("total error " + i + ": " + maxError)
    }
  }


  def main(args: Array[String]) {
    FPF.globalBitLength = 16

    println("bspline1:")
    useIntervalSubdivision(bspline1, bspline1_inputs, bspline1_divideOn)
    println("bspline2:")
    useIntervalSubdivision(bspline2, bspline2_inputs, bspline2_divideOn)
    println("log_poly:")
    useIntervalSubdivision(log_poly, log_poly_inputs, log_poly_divideOn)
    println("field_dc:")
    useIntervalSubdivision(field_dc_motor, field_dc_motor_inputs, field_dc_motor_divideOn)
  }

  val log_poly = new Array[String](4)
  // score = 0.000445 (interval 5: 0.000329)  (simulation: 0.0001176187)
  log_poly(0) = "(((-0.5 * (x * x)) + (x + ((x * (x * x)) * 0.333))) + (-0.25 * (x * (x * (x * x)))))"
  // score = 0.000399 (interval 5: 0.000284)  (simulation: 0.0001043258)
  log_poly(1) = "(((-0.5 * (x * x)) + x) + ((0.333 * (x * (x * x))) + ((-0.25 * (x * x)) * (x * x))))"
  // score = 0.000384 (interval 5: 0.000268)  (simulation: 0.0000738083)
  log_poly(2) = "x - 0.5*(x*x) + 0.333*(x*x*x) - 0.25*(x*x*x*x)"
  // score = 0.000314 (interval 5: 0.000251)  (simulation: 0.0001392772)
  log_poly(3) = "x * (1.0 + x * (-0.5 + x * (0.333 - 0.25 * x)))"
  var log_poly_inputs = new HashMap[String, (FPF, Interval)]
  log_poly_inputs += (("x", (FPF(16, 14), Interval(0.0, 1.0))))
  //log_poly_inputs += (("x", (FPF(32, 30), Interval(0.0, 1.0))))
  //log_poly_inputs += (("x", (FPF(24, 22), Interval(0.0, 1.0))))
  val log_poly_divideOn = List("x")

  val bspline1 = new Array[String](4)
  // score = 0.000510 (interval 5: 0.000363)  (simulation: 0.0001029644)
  bspline1(0) ="((((3.0 * (x * (x * x))) + (-6.0 * (x * x))) * 0.1666) + (4.0 * 0.1666))"
  // score = 0.000459 (interval 5: 0.000334)  (simulation: 0.0001221429 )
  bspline1(1) = "((((3.0 * (x * (x * x))) + (-6.0 * (x * x))) + 4.0) * 0.1666)"
  // score = 0.000365 (interval 5: 0.000256)  (simulation: 0.0001031062)
  bspline1(2) = "((4.0 + (((3.0 * x) + -6.0) * (x * x))) * 0.1666)"
  // score = 0.000311 (interval 5: 0.000229)  (simulation: 0.0001273727 )
  bspline1(3) = "((4.0 + (((-6.0 * x) + (x * (3.0 * x))) * x)) * 0.1666)"
  var bspline1_inputs = new HashMap[String, (FPF, Interval)]()
  bspline1_inputs += (("x", (FPF(16, 14), Interval(0.0, 1.0))))
  //bspline1_inputs += (("x", (FPF(32, 30), Interval(0.0, 1.0))))
  //bspline1_inputs += (("x", (FPF(24, 22), Interval(0.0, 1.0))))
  var bspline1_divideOn = List("x")

  val bspline2 = new Array[String](5)   // fairly good here!
  // score = 0.000477 (interval 5: 0.000336)  (simulation: 0.0001264192)
  bspline2(0) = "(0.1666 * (((-3.0 * (x * (x * x))) + ((3.0 * (x * x)) + (3.0 * x))) + 1.0))"
  // score = 0.000436 (interval 5: 0.000315)  (simulation: 0.0001101405)
  bspline2(1) = "(((((-3.0 * (x * (x * x))) + (3.0 * (x * x))) + (3.0 * x)) + 1.0) * 0.1666)"
  // score = 0.000246 (interval 5: 0.000213)  (simulation: 0.0001058646)
  bspline2(2) = "(((3.0 * x) + (1.0 + (x * (x + (x * (x * -3.0)))))) * 0.1666)"
  // score = 0.000236 (interval 5: 0.000193)  (simulation: 0.0000776532)
  bspline2(3) = "(0.1666 * (1.0 + (x * ((x + (-3.0 * (x * x))) + 3.0))))"
  // score = 0.000226 (interval 5: 0.000182)  (simulation: 0.0000968874)
  bspline2(4) = "((1.0 + ((3.0 + (((x * -3.0) * x) + x)) * x)) * 0.1666)"
  var bspline2_inputs = new HashMap[String, (FPF, Interval)]()
  bspline2_inputs += (("x", (FPF(16, 14), Interval(0.0, 1.0))))
  //bspline2_inputs += (("x", (FPF(32, 30), Interval(0.0, 1.0))))
  //bspline2_inputs += (("x", (FPF(24, 22), Interval(0.0, 1.0))))
  var bspline2_divideOn = List("x")

  val field_dc_motor = new Array[String](5)
  // score = 0.041567  (interval 5: 0.042384)  (simulation: 0.0038966290)
  field_dc_motor(3) = "((1/((epsilon + (theta * ia))) * (theta * (((b + a) * (ia * if)) + (rho * if)))) + (1/((epsilon + (theta * ia))) * -(c * ((if * if) * (omega * theta)))))"
  // score = 0.033840  (interval 5: 0.032372)  (simulation: 0.0033997779)
  field_dc_motor(0) = "(1/((epsilon + (theta * ia))) * (((theta * ((a + b) * (if * ia))) + (theta * (rho * if))) + -(c * (((if * if) * omega) * theta))))"
  // score = 0.033636  (interval 5: 0.032168)  (simulation: 0.0035076773)
  field_dc_motor(1) = "(1/((epsilon + (theta * ia))) * ((theta * (((a + b) * (if * ia)) + (rho * if))) + -(c * (((if * if) * omega) * theta))))"
  // score = 0.033432  (interval 5: 0.031964)  (simulation: 0.0035132674)
  field_dc_motor(2) = "(1/((epsilon + (theta * ia))) * ((theta * ((((b + a) * if) * ia) + (rho * if))) + -(c * (((if * if) * omega) * theta))))"
  // score = 0.033126  (interval 5: 0.031658)  (simulation: 0.0037285267)
  field_dc_motor(4) = "(1/(((theta * ia) + epsilon)) * ((theta * ((((b * if) + (if * a)) * ia) + (rho * if))) + -(c * (((if * if) * omega) * theta))))"
  var field_dc_motor_inputs = new HashMap[String, (FPF, Interval)]
  field_dc_motor_inputs += (("if", (FPF(16, 14), Interval(0.0, 1.5))));
  field_dc_motor_inputs += (("ia", (FPF(16, 14), Interval(0.2, 1.5))));
  field_dc_motor_inputs += (("omega", (FPF(16, 14), Interval(0.0, 1.5))));
  field_dc_motor_inputs += (("theta", (FPF(16, 14), Interval(1.0, 1.0))));
  field_dc_motor_inputs += (("rho", (FPF(16, 14), Interval(1.0, 1.0))));
  field_dc_motor_inputs += (("c", (FPF(16, 14), Interval(1.0, 1.0))));
  field_dc_motor_inputs += (("epsilon", (FPF(16, 18), Interval(0.1, 0.1))));
  field_dc_motor_inputs += (("a", (FPF(16, 14), Interval(1.0, 1.0))));
  field_dc_motor_inputs += (("b", (FPF(16, 14), Interval(1.0, 1.0))));
  val field_dc_motor_divideOn = List("if", "ia", "omega")

  val hermite = new Array[String](9)
  // score = 1.507893 (interval 5: 1.404096)  (simulation: 0.7002716064)
  hermite(0) = "(((((((((((x * x) * x) * x) * x) * x) * x) * x) + (-28.0 * (((((x * x) * x) * x) * x) * x))) + (210.0 * (((x * x) * x) * x))) + (-420.0 * (x * x))) + 105.0)"
  // score = 1.518670 (interval 5: 1.414873)  (simulation: 0.7266392261)
  hermite(1) = "((((((((((x * x) * x) * x) * x) * (x * x)) * x) + (-28.0 * (((((x * x) * x) * x) * x) * x))) + (210.0 * (((x * x) * x) * x))) + (-420.0 * (x * x))) + 105.0)"
  // score = 1.489373 (interval 5: 1.385576)  (simulation: 0.6834185123)
  hermite(2) = "((((((((((x * x) * x) * x) * x) * x) * (x * x)) + (-28.0 * (((((x * x) * x) * x) * x) * x))) + (210.0 * (((x * x) * x) * x))) + (-420.0 * (x * x))) + 105.0)"
  // score = 1.461018 (interval 5: 1.357221)  (simulation: 0.6663830578)
  hermite(3) = "((((((((((x * x) * x) * x) * x) * x) * x) * x) + ((-28.0 * (((((x * x) * x) * x) * x) * x)) + (210.0 * (((x * x) * x) * x)))) + (-420.0 * (x * x))) + 105.0)"
  // score = 1.458291 (interval 5: 1.354494)  (simulation: 0.7032444477)
  hermite(4) = "(((((((((x * x) * (x * x)) * x) * x) * x) * x) + (-28.0 * (((((x * x) * x) * x) * x) * x))) + ((210.0 * (((x * x) * x) * x)) + (-420.0 * (x * x)))) + 105.0)"
  // score = 1.446675 (interval 5: 1.342878)  (simulation: 0.6523900032)
  hermite(5) = "((((((x * ((((x * x) * x) * x) * x)) * x) * x) + (-28.0 * (((((x * x) * x) * x) * x) * x))) + ((((x * x) * x) * (x * 210.0)) + (-420.0 * (x * x)))) + 105.0)"
  // score = 1.366576 (interval 5: 1.262779)  (simulation: 0.6522526741)
  hermite(6) = "((((((((x * x) * x) * x) * x) * x) * (x * x)) + (-28.0 * (((((x * x) * x) * x) * x) * x))) + (((-420.0 * (x * x)) + (210.0 * ((x * x) * (x * x)))) + 105.0))"
  //  score = 1.366326 (interval 5: 1.262529)  (simulation: 0.6375482678)
  hermite(7) = "(((((((x * (x * x)) * x) * x) * x) * (x * x)) + (-28.0 * (((((x * x) * x) * x) * x) * x))) + (((-420.0 * (x * x)) + (210.0 * ((x * (x * x)) * x))) + 105.0))"
  // score = 1.366326 (interval 5: 1.262529)  (simulation: 0.6408477873)
  hermite(8) = "((((((((x * x) * x) * x) * x) * x) * (x * x)) + (-28.0 * (((((x * x) * x) * x) * x) * x))) + (((-420.0 * (x * x)) + (210.0 * (((x * x) * x) * x))) + 105.0))"
  var hermite_inputs = new HashMap[String, (FPF, Interval)]()
  hermite_inputs += (("x", (FPF(32, 27), Interval(-6.0, 10.0))))
  val hermite_divideOn = List("x")

  val multi_poly = new Array[String](9)
  // score = 0.019752 (interval 5: 0.019752)  (simulation: 0.0091508420)
  multi_poly(0) = "((((c + (a * b)) + 10.0) * (((a * c) + b) + 30.0)) * (((b * c) + a) + 20.0))"
  // score = 0.020347 (interval 5: 0.020347)  (simulation: 0.0101205776)
  multi_poly(1) = "(((c + (a * b)) + 10.0) * ((((a * c) + b) + 30.0) * (((b * c) + a) + 20.0)))"
  // score = 0.019031 (interval 5: 0.019031)  (simulation: 0.0089640780)
  multi_poly(2) = "(((((a * b) + c) + 10.0) * (((a * c) + b) + 30.0)) * ((b * c) + (a + 20.0)))"
  // score = 0.018701 (interval 5: 0.018701)  (simulation: 0.0096053425)
  multi_poly(3) = "(((((c + (a * b)) + 10.0) * ((a * c) + b)) + (((c + (a * b)) + 10.0) * 30.0)) * ((b * c) + (a + 20.0)))"
  // score = 0.018926 (interval 5: 0.018926)  (simulation: 0.0096058522)
  multi_poly(4) = "(((((a * b) + c) * (((a * c) + b) + 30.0)) + (10.0 * (((c * a) + b) + 30.0))) * ((b * c) + (a + 20.0)))"
  // score = 0.018532 (interval 5: 0.018532)  (simulation: 0.0099191156)
  multi_poly(5) = "(((((c + (b * a)) + 10.0) * ((a * c) + b)) + (((b * a) + (c + 10.0)) * 30.0)) * ((c * b) + (20.0 + a)))"
  // score = 0.018021 (interval 5: 0.018021)  (simulation: 0.0091486687)
  multi_poly(6) = "((((((c + (b * a)) + 10.0) * (a * c)) + (((c + (b * a)) + 10.0) * b)) + (30.0 * ((a * b) + (c + 10.0)))) * ((b * c) + (a + 20.0)))"
  // score = 0.018281 (interval 5: 0.018281)  (simulation: 0.0099133553)
  multi_poly(7) = "((((((c + (b * a)) + 10.0) * (a * c)) + (b * ((c + (b * a)) + 10.0))) * ((b * c) + (a + 20.0))) + ((30.0 * ((a * b) + (c + 10.0))) * ((b * c) + (a + 20.0))))"
  // score = 0.017240 (interval 5: 0.017240)  (simulation: 0.0090513302)
  multi_poly(8) = "((((((c + (b * a)) * a) + (10.0 * a)) * c) + ((((c + (b * a)) + 10.0) * b) + ((30.0 * (a * b)) + (30.0 * (c + 10.0))))) * ((c * b) + (a + 20.0)))"

  var multi_poly_inputs = new HashMap[String, (FPF, Interval)]()
  multi_poly_inputs += (("a", (FPF(32, 27), Interval(-10.0, 10.0))))
  multi_poly_inputs += (("b", (FPF(32, 27), Interval(-6.0, 8.0))))
  multi_poly_inputs += (("c", (FPF(32, 27), Interval(0.0, 12.0))))
  val multi_poly_divideOn = List("a", "b", "c")

  // The code generation doesn't seem to work here...
  val doppler = new Array[String](4)
  // score = 0.003377 (interval 5: 0.003377)  (simulation: )
  doppler(0) = "(((331.4 + (0.6 * t)) * f) * 1/((((331.4 + (0.6 * t)) + u) * ((331.4 + (0.6 * t)) + u))))"
  // score = 0.003376 (interval 5: 0.003376)  (simulation: )
  doppler(1) = "(((331.4 + (0.6 * t)) * f) * 1/(((331.4 + ((0.6 * t) + u)) * (331.4 + ((0.6 * t) + u)))))"
  // score = 0.003399 (interval 5: 0.003431)  (simulation: )
  doppler(2) = "(((331.4 + (t * 0.6)) * f) * (1/(((331.4 + (t * 0.6)) + u)) * 1/(((331.4 + (t * 0.6)) + u))))"
  // score = 0.003385 (interval 5: 0.006751)  (simulation: ) ????  <<-- this looks like a bug
  doppler(3) = "((1/(((331.4 + ((0.6 * t) + u)) * (331.4 + (u + (0.6 * t))))) * ((0.6 * t) + 331.4)) * f)"
  var doppler_inputs = new HashMap[String, (FPF, Interval)]()
  doppler_inputs += (("t", (FPF(32, 25), Interval(-30.0, 50.0))))
  doppler_inputs += (("f", (FPF(32, 16), Interval(20.0, 20000))))
  doppler_inputs += (("u", (FPF(32, 24), Interval(-100.0, 100.0))))
  val doppler_divideOn = List("t", "f", "u")

  val dc_motor = new Array[String](5)
  dc_motor(0) = "(1 * In1) - ((-2.6) * In2 + ((0.0864 * In3) + ((-0.005) * In4)))"
  dc_motor(1) = "((1 * In1) - ((((-2.6) * In2) + (0.0864 * In3)) + ((-0.005) * In4)))"
  dc_motor(2) = "((((1 * In1) - ((-2.6) * In2)) - (0.0864 * In3)) - ((-0.005) * In4))"
  dc_motor(3) = "((((1 * In1) + (2.6 * In2)) - (0.0864 * In3)) + (0.005 * In4))"
  dc_motor(4) = "(((1 * In1) - (0.0864 * In3)) - (((-2.6) * In2) + ((-0.005) * In4)))"
  var dc_motor_inputs = new HashMap[String, (FPF, Interval)]
  dc_motor_inputs += (("In1", (FPF(16, 8), Interval(0, 100))))
  dc_motor_inputs += (("In2", (FPF(16, 9), Interval(-10.2379, 61.3396))))
  dc_motor_inputs += (("In3", (FPF(16, 4), Interval(0, 1207.0822))))
  dc_motor_inputs += (("In4", (FPF(16, 5), Interval(-32.1131, 734.9213))))
  val dc_motor_divideOn = List("In1", "In2", "In3", "In4")



}
