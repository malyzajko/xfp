package xfp.simulation

import xfp.fixedpoint.{FixedPointFormat => FPFormat, Interval}
import xfp.utils.{BigRational => Rational}
import Rational._

trait Simulation {
  val r = new scala.util.Random(System.currentTimeMillis)
  val n = 10000
  val repeats = 100


  // Get the integer bitvector for the given number of fractional bits
  def doubleToInt(d: Double, f: Int): Int = {
    return (d * math.pow(2, f)).round.toInt
  }
  def intToDouble(i: Int, f: Int): Double = {
    return i.toDouble / math.pow(2, f)
  }
  def doubleToLong(d: Double, f: Int): Long = {
    return (d * math.pow(2, f)).round.toLong
  }
  def longToDouble(i: Long, f: Int): Double = {
    return i.toDouble / math.pow(2, f)
  }

  def findLowerBound(dfnc: (Double) => Double, ffnc: (Int) => Int, format: FPFormat, range: Interval,
    outputFrac: Int): (Double, Double) = {
    var maxError = 0.0
    var count = 0 // counts how many times the close error is larger
    var total = 0
    val p_lo = math.ceil(range.xlo * math.pow(2, format.f)).toInt
    val p_hi = math.floor(range.xhi * math.pow(2, format.f)).toInt
    val factor = Rational(1.0)/ Rational(math.pow(2, format.f).toInt)
    val quantError = format.quantError
    //println("p_lo: " + p_lo + "   p_hi: " + p_hi)
    //println("factor: " + factor)
    var p = p_lo
    while (p < p_hi) {
      val repr = factor * Rational(p)
      val close = repr + Rational(0.01) * quantError
      val far = repr + Rational(0.95) * quantError
      assert(close.toDouble >= range.xlo && close.toDouble <= range.xhi)
      assert(far.toDouble >= range.xlo && far.toDouble <= range.xhi)
      val closeError = getError(dfnc, ffnc, close.toDouble, format.f, outputFrac)
      val farError = getError(dfnc, ffnc, far.toDouble, format.f, outputFrac)
      if (closeError > farError) count += 1
      //println("close: " + closeError + ",  far: " + farError)
      maxError = math.max(maxError, math.max(closeError, farError))
      p += 1
      total += 1
    }
    print("count " + count + "   total " + total + "   ")
    return (maxError, 0.0)
  }

  def getError(dfnc: (Double) => Double, ffnc: (Int) => Int, x: Double, f: Int, o: Int) = {
    val dbl = dfnc(x)
    val intgr = ffnc(doubleToInt(x, f))
    math.abs(dbl - intToDouble(intgr, o))
  }

  def findMaxError(dfnc: (Double) => Double, ffnc: (Int) => Int, inputFormat: FPFormat, inputRange: Interval,
    outputFrac: Int, stepSize: Double): (Double, Double) = {
    var maxError = 0.0
    var maxX = inputRange.xlo
    var currX = inputRange.xlo
    while (currX <= inputRange.xhi) {
      val dbl = dfnc(currX)
      val intgr = ffnc(doubleToInt(currX, inputFormat.f))
      val error = math.abs(dbl - intToDouble(intgr, outputFrac))
      if (error > maxError) {
        maxX = currX
        maxError = error
      }
      currX += stepSize
    }
    println("At stepSize " + stepSize + "  largest error at " + maxX + "   = " + maxError)
    if (stepSize > inputFormat.quantError.toDouble) {
      findMaxError(dfnc, ffnc, inputFormat, Interval(maxX - stepSize, maxX + stepSize), outputFrac, stepSize / 2.0)
    }
    else {
      return (maxError, maxX)
    }
  }



  // Looks like we'll have to do this for functions for different number of args separately
  def runSimulation1(inputFormat: FPFormat, inputRange: Interval, outputFrac: Int, dfnc: (Double) => Double,
    ffnc: (Int) => Int, n: Int, repeats: Int) = {
    var maxError = 0.0
    for (i <- 0 until repeats) {
      for (i <- 0 until n) {
        val x = inputRange.xlo + r.nextDouble * (inputRange.xhi - inputRange.xlo)
        val dbl = dfnc(x)
        val intgr = ffnc(doubleToInt(x, inputFormat.f))
        maxError = math.max(maxError, math.abs(dbl - intToDouble(intgr, outputFrac)))
      }
    }
    println("maxError = %.10f".format(maxError))
  }

  def runSimulation1Long(inputFormat: FPFormat, inputRange: Interval, outputFrac: Int, dfnc: (Double) => Double,
    ffnc: (Long) => Long, n: Int, repeats: Int) = {
    var maxError = 0.0
    for (i <- 0 until repeats) {
      for (i <- 0 until n) {
        val x = inputRange.xlo + r.nextDouble * (inputRange.xhi - inputRange.xlo)
        val dbl = dfnc(x)
        val intgr = ffnc(doubleToLong(x, inputFormat.f))
        maxError = math.max(maxError, math.abs(dbl - longToDouble(intgr, outputFrac)))
      }
    }
    println("maxError = %.10f".format(maxError))
  }

  def runSimulation2(inputFormats: List[FPFormat], inputRanges: List[Interval], outputFrac: Int,
    dfnc: (Double, Double) => Double, ffnc: (Int, Int) => Int, n: Int, repeats: Int) = {
    var maxError = 0.0
    for (i <- 0 until repeats) {
      for (i <- 0 until n) {
        val x1 = inputRanges(0).xlo + r.nextDouble * (inputRanges(0).xhi - inputRanges(0).xlo)
        val x2 = inputRanges(1).xlo + r.nextDouble * (inputRanges(1).xhi - inputRanges(1).xlo)
        val dbl = dfnc(x1, x2)
        val intgr = ffnc(doubleToInt(x1, inputFormats(0).f), doubleToInt(x2, inputFormats(1).f))
        maxError = math.max(maxError, math.abs(dbl - intToDouble(intgr, outputFrac)))
      }
    }
    println("maxError = %.10f".format(maxError))
  }

  def runSimulation2Long(inputFormats: List[FPFormat], inputRanges: List[Interval], outputFrac: Int,
    dfnc: (Double, Double) => Double, ffnc: (Long, Long) => Long, n: Int, repeats: Int) = {
    var maxError = 0.0
    for (i <- 0 until repeats) {
      for (i <- 0 until n) {
        val x1 = inputRanges(0).xlo + r.nextDouble * (inputRanges(0).xhi - inputRanges(0).xlo)
        val x2 = inputRanges(1).xlo + r.nextDouble * (inputRanges(1).xhi - inputRanges(1).xlo)
        val dbl = dfnc(x1, x2)
        val intgr = ffnc(doubleToLong(x1, inputFormats(0).f), doubleToLong(x2, inputFormats(1).f))
        maxError = math.max(maxError, math.abs(dbl - longToDouble(intgr, outputFrac)))
      }
    }
    println("maxError = %.10f".format(maxError))
  }

  def runSimulation3(inputFormats: List[FPFormat], inputRanges: List[Interval], outputFrac: Int,
    dfnc: (Double, Double, Double) => Double, ffnc: (Int, Int, Int) => Int, n: Int, repeats: Int) = {
    var maxError = 0.0
    for (i <- 0 until repeats) {
      for (i <- 0 until n) {
        val x1 = inputRanges(0).xlo + r.nextDouble * (inputRanges(0).xhi - inputRanges(0).xlo)
        val x2 = inputRanges(1).xlo + r.nextDouble * (inputRanges(1).xhi - inputRanges(1).xlo)
        val x3 = inputRanges(2).xlo + r.nextDouble * (inputRanges(2).xhi - inputRanges(2).xlo)
        val dbl = dfnc(x1, x2, x3)
        val intgr = ffnc(doubleToInt(x1, inputFormats(0).f), doubleToInt(x2, inputFormats(1).f),
                          doubleToInt(x3, inputFormats(2).f))
        maxError = math.max(maxError, math.abs(dbl - intToDouble(intgr, outputFrac)))
      }
    }
    println("maxError = %.10f".format(maxError))
  }

  def runSimulation3Long(inputFormats: List[FPFormat], inputRanges: List[Interval],
    outputFrac: Int, dfnc: (Double, Double, Double) => Double,
    ffnc: (Long, Long, Long) => Long) = {
    var maxError = 0.0
    for (i <- 0 until repeats) {
      for (i <- 0 until n) {
        val x1 = inputRanges(0).xlo + r.nextDouble * (inputRanges(0).xhi - inputRanges(0).xlo)
        val x2 = inputRanges(1).xlo + r.nextDouble * (inputRanges(1).xhi - inputRanges(1).xlo)
        val x3 = inputRanges(2).xlo + r.nextDouble * (inputRanges(2).xhi - inputRanges(2).xlo)
        val dbl = dfnc(x1, x2, x3)
        var error = 100000.0
        try {
          val intgr = ffnc(doubleToLong(x1, inputFormats(0).f), doubleToLong(x2, inputFormats(1).f),
                          doubleToLong(x3, inputFormats(2).f))
          error = math.abs(dbl - longToDouble(intgr, outputFrac))
        } catch {
          case e: Exception => ;
        }

        maxError = math.max(maxError, error)
      }
    }
    println("maxError = %.10f".format(maxError))
  }

  def runSimulation5Long(inputFormats: List[FPFormat], inputRanges: List[Interval],
    outputFrac: Int, dfnc: (Double, Double, Double, Double, Double) => Double,
    ffnc: (Long, Long, Long, Long, Long) => Long) = {
    var maxError = 0.0
    for (i <- 0 until repeats) {
      for (i <- 0 until n) {
        val x1 = inputRanges(0).xlo + r.nextDouble * (inputRanges(0).xhi - inputRanges(0).xlo)
        val x2 = inputRanges(1).xlo + r.nextDouble * (inputRanges(1).xhi - inputRanges(1).xlo)
        val x3 = inputRanges(2).xlo + r.nextDouble * (inputRanges(2).xhi - inputRanges(2).xlo)
        val x4 = inputRanges(3).xlo + r.nextDouble * (inputRanges(3).xhi - inputRanges(3).xlo)
        val x5 = inputRanges(4).xlo + r.nextDouble * (inputRanges(4).xhi - inputRanges(4).xlo)
        val dbl = dfnc(x1, x2, x3, x4, x5)
        var error = 100000.0
        try {
          val intgr = ffnc(doubleToLong(x1, inputFormats(0).f), doubleToLong(x2, inputFormats(1).f),
                          doubleToLong(x3, inputFormats(2).f), doubleToLong(x4, inputFormats(3).f),
                          doubleToLong(x5, inputFormats(4).f))
          error = math.abs(dbl - longToDouble(intgr, outputFrac))
        } catch {
          case e: Exception => ;
        }

        maxError = math.max(maxError, error)
      }
    }
    println("maxError = %.10f".format(maxError))
  }
}
