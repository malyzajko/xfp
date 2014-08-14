package xfp.fixedpoint

import AffineUtils._
import xfp.utils.{BigRational => Rational}
import Rational._
import FixedPointFormat._
import FixedForm._
import collection.mutable.Queue

object FixedForm {
  def apply(interval: Interval, format: FixedPointFormat): FixedForm = {
    val a = Rational(interval.xlo)
    val b = Rational(interval.xhi)
    val un = (b - a)/ Rational(2)
    new FixedForm(format, a + un, un)
  }

  var currIndex: Int = 0
  def newIndex: Int = {
    currIndex += 1
    assert(currIndex != Int.MaxValue, "Affine indices just ran out...")
    currIndex
  }
}

/*
 The differences to BaseFixedForm are:
  - a little smarter computation of non-linear part for multiplication
  - constants have no errors if they are exact
*/
case class FixedForm(val format: FixedPointFormat, val r0: Rational, var rnoise: Queue[Deviation],
  var rerror: Queue[Deviation]) {

  // Constant value
  def this(f: FixedPointFormat, r: Rational) = {
    this(f, r, new Queue[Deviation], new Queue[Deviation])
    if (!f.canRepresent(r))
      rerror += Deviation(FixedForm.newIndex, f.quantError)


    println("constant: " + r)
    println("format: " + f)
    println("error: " + rerror)
  }

  // Creating a range of values in fixed point format
  def this(f: FixedPointFormat, r: Rational, un: Rational) = {
    this(f, r, new Queue[Deviation]() += Deviation(FixedForm.newIndex, un), new Queue[Deviation])
    rerror += Deviation(FixedForm.newIndex, f.quantError)
  }

  // Constant
  def this(r: Rational) = this(FixedPointFormat.getFormat(r), r)
  def this(d: Double) = this(Rational(d))

  // Range of values
  def this(r: Rational, un: Rational) =
    this(FixedPointFormat.getFormat(r - un, r + un), r, un)

  if (rnoise.size > maxNoiseCount) {
    println("Packing noise terms")
    rnoise = packRationalNoiseTerms(rnoise)
  }
  if (rerror.size > maxNoiseCount) {
    println("Packing error terms")
    rerror = packRationalNoiseTerms(rerror)
  }

  // Check if the format can hold the range.
  if(checkForOverflow && !format.includesRange(qInterval) ) {
    val msg = "Format %s, [%s, %s] cannot hold range [%s, %s]".format(format.toString,
        format.range._1.toString, format.range._2.toString,
        this.qInterval._1.toString, this.qInterval._2.toString)
    println("Overflow exception: " + msg)
    throw FixedPointOverflowException(msg)
  }

  // Radius as computed in reals
  def qRealRadius: Rational = sumQueue(rnoise)

  /** The radius of values in R represented by this fixed-point number. */
  def qRadius: Rational = sumQueue(rnoise) +  sumQueue(rerror)

  /** This is the interval of values in R represented by this fixed-point range. */
  def qInterval: (Rational, Rational) = {
    val rad = qRadius
    (r0 - rad, r0 + rad)
  }

  def interval = Interval(qInterval._1.toDouble, qInterval._2.toDouble)

  /** The maximum absolute error commited from the calculation done in reals. */
  def maxAbsError: Rational = sumQueue(rerror)

  override def toString: String =
    "[%f,%f] %s %g".format(interval.xlo, interval.xhi, format.toString, maxAbsError.toDouble)
    //r0.toDouble.toString + " " + formatQueue(rnoise) + " " + formatQueue(rerror, true)

  def absValue: FixedForm = {
    if (Rational(0) <= r0) return this else return -this
  }

  def isNonZero: Boolean = return (r0 != 0 || rnoise.size > 0 || rerror.size > 0)

  /**
    Negates this FixedForm. Assumes signed format.
   */
  def unary_-(): FixedForm = {
    if (!format.signed)
      throw IncompatibleFixedPointFormatsException("Unary minus not supported with unsigned format!")
    var newTerms = new Queue[Deviation]()
    var iter = rnoise.iterator
    while(iter.hasNext) {
      newTerms += - iter.next  // just flip the sign
    }
    var newErrors = new Queue[Deviation]()
    iter = rerror.iterator
    while(iter.hasNext) {
      newErrors += - iter.next  // just flip the sign
    }
    new FixedForm(format, -r0, newTerms, newErrors)
  }

  /**
    Computes the sum and uses as the result format the tightest one,
    while trying to avoid overflow. Throws an exception if overflow cannot be avoided.
    Both operands as well as the result are assumed to have the same number of bits.
   */
  def +(y: FixedForm): FixedForm = this.plus(y)

  /**
    Computes the sum and uses the given format for the result, if any is given.
   */
  def plus(y: FixedForm, givenFormat: FixedPointFormat = null): FixedForm = {
    assert(this.format.bits == y.format.bits)
    println(this.r0 + "  +  " + y.r0)
    val z0 = this.r0 + y.r0
    val newTerms = addQueues(this.rnoise, y.rnoise)
    val newErrors = addQueues(this.rerror, y.rerror)
    val newFormat = if (givenFormat != null) givenFormat else getFormat(z0, newTerms, newErrors, format.bits)
    /*println("newFormat.f: " + newFormat.f)
      println("this.format.f: " + this.format.f)
      println("y.format.f: " + y.format.f)*/
    if (newFormat.f < math.max(this.format.f, y.format.f)) { // we're loosing precision
      newErrors += Deviation(newIndex, newFormat.quantError)
      println("add, new error: " + newFormat.quantError)

    }
    //println("addition " + newErrors)
    return new FixedForm(newFormat, z0, newTerms, newErrors)
  }

  /**
    Computes the difference and uses as the result format the tightest one,
    while trying to avoid overflow. Throws an exception if overflow cannot be avoided.
    Both operands as well as the result are assumed to have the same number of bits.
   */
  def -(y: FixedForm): FixedForm = this.minus(y)

  /**
    Computes the difference and uses the given format for the result, if any is given.
   */
  def minus(y: FixedForm, givenFormat: FixedPointFormat = null): FixedForm = {
    assert(this.format.bits == y.format.bits)
    val z0 = this.r0 - y.r0
    val newTerms = subtractQueues(this.rnoise, y.rnoise)
    val newErrors = subtractQueues(this.rerror, y.rerror)

    val newFormat = if (givenFormat != null) givenFormat else getFormat(z0, newTerms, newErrors, format.bits)
    if (newFormat.f < math.max(this.format.f, y.format.f)) { // we're loosing precision
      newErrors += Deviation(newIndex, newFormat.quantError)
    }
    //println("subtraction " + newErrors)
    return new FixedForm(newFormat, z0, newTerms, newErrors)
  }

  /**
    Computes the product and uses as the result format the tightest one,
    while trying to avoid overflow. Throws an exception if overflow cannot be avoided.
    Both operands as well as the result are assumed to have the same number of bits.
   */
  def *(y: FixedForm): FixedForm = this.times(y)

  /**
    Computes the product and uses as the given format for the result, if any is given.
   */
  def times(y: FixedForm, givenFormat: FixedPointFormat = null): FixedForm = {
    assert(this.format.bits == y.format.bits)
    //println(this.r0 + "  *  " + y.r0)
    var z0 = this.r0 * y.r0
    //println("z0: " + z0)
    var (z0Addition, delta) = multiplyNonlinearQueues2(this.rnoise, y.rnoise)
    //println("z0Addition: " + z0Addition)
    //println("delta: " + delta)
    z0 += z0Addition
    val newTerms = multiplyQueues(this.r0, this.rnoise, y.r0, y.rnoise)
    if(delta != 0)
      newTerms += Deviation(newIndex, delta)

    // linear part of errors:
    val newErrors = multiplyQueues(this.r0, this.rerror, y.r0, y.rerror)
    // faster and doesn't seem to make a difference
    val rx_ey = sumQueue(this.rnoise) * sumQueue(y.rerror)
    val ry_ex = sumQueue(y.rnoise) * sumQueue(this.rerror)
    //val rx_ey = multiplyNonlinearQueues(this.rnoise, y.rerror)
    //val ry_ex = multiplyNonlinearQueues(y.rnoise, this.rerror)
    val ex_ey = multiplyNonlinearQueues(this.rerror, y.rerror)
    var nonlinearDelta = rx_ey + ry_ex + ex_ey
    if (nonlinearDelta != 0) {
      newErrors += Deviation(newIndex, nonlinearDelta)
    }

    val newFormat = if (givenFormat != null) givenFormat else getFormat(z0, newTerms, newErrors, format.bits)
    if (newFormat.f < this.format.f + y.format.f) { //loosing precision
      if (!exactConstantMultiplication || (this.qRadius != 0.0 && y.qRadius != 0.0))
        newErrors += Deviation(newIndex, newFormat.quantError)
        //println("mult, new error: " + newFormat.quantError + " f: " + newFormat.f)
    }
    //println("multiplication " + newErrors)
      return new FixedForm(newFormat, z0, newTerms, newErrors)
  }

  /**
    Computes the inverse of this FixedForm as a linear approximation.
   */
  def inverse(givenFormat: FixedPointFormat = null): FixedForm = {
    val (xlo, xhi) = qInterval

    if (xlo <= Rational(0.0) && xhi >= Rational(0.0))
      throw DivisionByZeroException("Possible division by zero: " + toString)

    if(rnoise.size == 0.0) { //exact
      val inv = Rational(1.0)/r0
      val f = getFormat(inv)
      val errors = new Queue[Deviation]()
      errors += Deviation(newIndex, f.quantError)
      return new FixedForm(f, inv, new Queue[Deviation](), errors)
    }

    /* Calculate the inverse */
    val a = min(abs(xlo), abs(xhi))
    val b = max(abs(xlo), abs(xhi))
    //println("a "  + a)
    val alpha = Rational(-1.0) / (b * b)

    val errorMultiplier =
      if (divisionHack) max(Rational(-1.0) / (a * a), Rational(-100.0))
      else Rational(-1.0) / (a * a)

    //println("errorMultiplier " + errorMultiplier)
    val dmax = (Rational(1.0) / a) - (alpha * a)
    val dmin = (Rational(1.0) / b) - (alpha * b)

    var zeta = (dmin / Rational(2.0)) + (dmax / Rational(2.0))
    if (xlo < Rational(0.0)) zeta = -zeta
    val delta = max( zeta - dmin, dmax - zeta )

    // TODO: it may be possible to subtract the new error from delta to get more precision
    val z0 = alpha * this.r0 + zeta

    var newTerms = multiplyQueue(rnoise, alpha)
    if(delta != 0.0) newTerms += new Deviation(newIndex, delta)

    var newErrors = multiplyQueue(rerror, errorMultiplier)

    // Compute result format based on range of result
    val newFormat = if(givenFormat != null) givenFormat else getFormat(z0, newTerms, newErrors, format.bits)
    assert(newFormat.bits == this.format.bits,
      "New format has wrong number of bits %d (vs %d)".format(newFormat.bits, this.format.bits))
    /*
      We may not need to add this error here, since this operation is not actually performed
      in fixed-point computations. We just need to do it to compute the propagation of the errors.
    */
    newErrors += Deviation(newIndex, newFormat.quantError)
    //println("inverse " + newErrors)
    return new FixedForm(newFormat, z0, newTerms, newErrors)
    //return unaryOp(this.r0, this.rnoise, alpha, zeta, delta, this.rerror, errorMultiplier)
  }

  /**
   Computes x/y as x * (1/y).
   */
  def /(y: FixedForm): FixedForm = {
    return this * y.inverse()
  }

  def divide(y: FixedForm, newFormat: FixedPointFormat): FixedForm = {
    return this.times(y.inverse(), newFormat)
  }

  private def multiplyNonlinearQueues(xqueue: Queue[Deviation], yqueue: Queue[Deviation]): Rational = {
    val indices = mergeIndices(getIndices(xqueue), getIndices(yqueue))
    var zqueue = Rational(0.0)

    var i = 0
    while (i < indices.length) {
      val iInd = indices(i)
      // quadratic
      val xi = xqueue.find((d: Deviation) => d.index == iInd) match {
        case Some(d) => d.value; case None => Rational(0) }
      val yi = yqueue.find((d: Deviation) => d.index == iInd) match {
        case Some(d) => d.value; case None => Rational(0) }
      val zii = xi * yi
      if (zii != 0) zqueue += abs(zii)

      var j = i + 1
      while (j < indices.length) {
        val jInd = indices(j)
        val xj = xqueue.find((d: Deviation) => d.index == jInd) match {
        case Some(d) => d.value; case None => Rational(0) }
        val yj = yqueue.find((d: Deviation) => d.index == jInd) match {
        case Some(d) => d.value; case None => Rational(0) }
        val zij = xi * yj + xj * yi
        if (zij != 0) zqueue += abs(zij)
        j += 1
      }
      i += 1
    }
    zqueue
  }

  // Does a smarter computation of the quadratic terms
  private def multiplyNonlinearQueues2(xqueue: Queue[Deviation], yqueue: Queue[Deviation]): (Rational, Rational) = {
    val indices = mergeIndices(getIndices(xqueue), getIndices(yqueue))
    var zqueue = Rational(0.0)
    var z0Addition = Rational(0.0)

    var i = 0
    while (i < indices.length) {
      val iInd = indices(i)
      // quadratic
      val xi = xqueue.find((d: Deviation) => d.index == iInd) match {
        case Some(d) => d.value; case None => Rational(0) }
      val yi = yqueue.find((d: Deviation) => d.index == iInd) match {
        case Some(d) => d.value; case None => Rational(0) }
      val zii = xi * yi
      z0Addition += zii / Rational(2.0)
      if (zii != 0) zqueue += abs(zii / Rational(2.0))

      var j = i + 1
      while (j < indices.length) {
        val jInd = indices(j)
        val xj = xqueue.find((d: Deviation) => d.index == jInd) match {
          case Some(d) => d.value; case None => Rational(0) }
        val yj = yqueue.find((d: Deviation) => d.index == jInd) match {
        case Some(d) => d.value; case None => Rational(0) }
        val zij = xi * yj + xj * yi
        if (zij != 0) zqueue += abs(zij)
        j += 1
      }
      i += 1
    }
    (z0Addition, zqueue)
  }

  private def mergeIndices(x: Set[Int], y: Set[Int]): Array[Int] = {
    val set = x ++ y
    val list = set.toList.sorted
    return list.toArray
  }

  private def packRationalNoiseTerms(queue: Queue[Deviation]): Queue[Deviation] = {
    var sum = sumQueue(queue)
    val avrg: Double = sum.toDouble / queue.size

    //compute st. deviation
    var devSum = 0.0
    val iter = queue.iterator
    while(iter.hasNext) {
      val diff = (abs(iter.next.value).toDouble - avrg).toDouble
      devSum += diff * diff
    }
    val stdDev = math.sqrt(devSum/queue.size)
    val threshold: Double = avrg + stdDev

    //Now compute the new queue
    var newNoise = Rational(0)
    var newDev = new Queue[Deviation]()

    val iter2 = queue.iterator
    while(iter2.hasNext) {
      val xi = iter2.next
      val v = abs(xi.value)
      if(v.toDouble < threshold) newNoise += v
      else newDev += xi
    }
    newDev += new Deviation(newIndex, newNoise)
    return newDev
  }

  //Multiplication
  //This doesn't seem to have much effect (like any), but keep this for later.
      //println("format computed based on affine: " + newFormat)
      //val xinterval = new RationalInterval(this.qInterval._1, this.qInterval._2)
      //val yinterval = new RationalInterval(y.qInterval._1, y.qInterval._2)
      //val zinterval = xinterval * yinterval
      //val iFormat = getFormat(zinterval.xlo, zinterval.xhi)
      //if (iFormat.f > newFormat.f) {
        //println("Interval produced more precise format %s - %s".format(iFormat.toString, newFormat.toString))
      //}
      //println("format computed based on interval " + intervalFormat)*/


}
