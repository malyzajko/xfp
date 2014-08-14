package xfp.simulation

import xfp.fixedpoint.{FixedPointFormat => FPFormat, Interval}

object TestExamples extends App with Simulation {

  //val err = findMaxError(bspline1_1D, bspline1_1I, FPFormat(16, 14), Interval(0.0, 1.0), 15, 0.1)
  var err = findLowerBound(bspline1_1D, bspline1_1I, FPFormat(16, 14), Interval(0.0, 1.0), 15)
  println("max error %1.8f".format(err._1))
  err = findLowerBound(bspline1_2D, bspline1_2I, FPFormat(16, 14), Interval(0.0, 1.0), 15)
  println("max error %1.8f".format(err._1))
  err = findLowerBound(bspline1_3D, bspline1_3I, FPFormat(16, 14), Interval(0.0, 1.0), 15)
  println("max error %1.8f".format(err._1))
  err = findLowerBound(bspline1_4D, bspline1_4I, FPFormat(16, 14), Interval(0.0, 1.0), 15)
  println("max error %1.8f".format(err._1))

  println("\n bspline2: ")
  err = findLowerBound(bspline2_1D, bspline2_1I, FPFormat(16, 14), Interval(0.0, 1.0), 15)
  println("max error %1.8f".format(err._1))
  err = findLowerBound(bspline2_2D, bspline2_2I, FPFormat(16, 14), Interval(0.0, 1.0), 15)
  println("max error %1.8f".format(err._1))
  err = findLowerBound(bspline2_3D, bspline2_3I, FPFormat(16, 14), Interval(0.0, 1.0), 15)
  println("max error %1.8f".format(err._1))
  err = findLowerBound(bspline2_4D, bspline2_4I, FPFormat(16, 14), Interval(0.0, 1.0), 15)
  println("max error %1.8f".format(err._1))
  err = findLowerBound(bspline2_5D, bspline2_5I, FPFormat(16, 14), Interval(0.0, 1.0), 15)
  println("max error %1.8f".format(err._1))


  def logPolynomial1D(x: Double): Double = x - 0.5*(x*x) + 0.333*(x*x*x) - 0.25*(x*x*x*x)
  def logPolynomial1I(x: Int): Int = {
    val tmp0 = ((x * x) >> 14)
    val tmp1 = ((16384 * tmp0) >> 14)
    val tmp2 = ((x << 1) - tmp1)
    val tmp3 = ((x * x) >> 14)
    val tmp4 = ((tmp3 * x) >> 14)
    val tmp5 = ((21823 * tmp4) >> 15)
    val tmp6 = (tmp2 + tmp5)
    val tmp7 = ((x * x) >> 14)
    val tmp8 = ((tmp7 * x) >> 14)
    val tmp9 = ((tmp8 * x) >> 14)
    val tmp10 = ((16384 * tmp9) >> 15)
    val tmp11 = (tmp6 - tmp10)
    return tmp11
  }
  def logPolynomial2D(x: Double): Double = x * (1.0 + x * (-0.5 + x * (0.333 - 0.25 * x)))
  def logPolynomial2I(x: Int): Int = {
    val tmp0 = ((16384 * x) >> 15)
    val tmp1 = ((21823 - (tmp0 << 1)) >> 1)
    val tmp2 = ((x * tmp1) >> 14)
    val tmp3 = (-16384 + tmp2)
    val tmp4 = ((x * tmp3) >> 14)
    val tmp5 = (((16384 << 1) + tmp4) >> 1)
    val tmp6 = ((x * tmp5) >> 13)
    return tmp6
  }
  def logPolynomial3D(x: Double): Double = (((-0.5 * (x * x)) + (x + ((x * (x * x)) * 0.333))) + (-0.25 * (x * (x * (x * x)))))
  def logPolynomial3I(x: Int): Int = {
    val tmp0 = ((x * x) >> 14)
    val tmp1 = ((-16384 * tmp0) >> 14)
    val tmp2 = ((x * x) >> 14)
    val tmp3 = ((x * tmp2) >> 14)
    val tmp4 = ((tmp3 * 21823) >> 15)
    val tmp5 = (((x << 1) + tmp4) >> 1)
    val tmp6 = (tmp1 + (tmp5 << 1))
    val tmp7 = ((x * x) >> 14)
    val tmp8 = ((x * tmp7) >> 14)
    val tmp9 = ((x * tmp8) >> 14)
    val tmp10 = ((-16384 * tmp9) >> 15)
    val tmp11 = (tmp6 + tmp10)
    return tmp11
  }
  def logPolynomial4D(x: Double): Double = (((-0.5 * (x * x)) + x) + ((0.333 * (x * (x * x))) + ((-0.25 * (x * x)) * (x * x))))
  def logPolynomial4I(x: Int): Int = {
    val tmp0 = ((x * x) >> 14)
    val tmp1 = ((-16384 * tmp0) >> 14)
    val tmp2 = (tmp1 + (x << 1))
    val tmp3 = ((x * x) >> 14)
    val tmp4 = ((x * tmp3) >> 14)
    val tmp5 = ((21823 * tmp4) >> 15)
    val tmp6 = ((x * x) >> 14)
    val tmp7 = ((-16384 * tmp6) >> 15)
    val tmp8 = ((x * x) >> 14)
    val tmp9 = ((tmp7 * tmp8) >> 14)
    val tmp10 = (tmp5 + tmp9)
    val tmp11 = (tmp2 + tmp10)
    return tmp11
  }


  def bspline1_1D(x: Double): Double = ((((3.0 * (x * (x * x))) + (-6.0 * (x * x))) * 0.1666) + (4.0 * 0.1666))
  def bspline1_1I(x: Int): Int = {
    val tmp0 = ((x * x) >> 14)
    val tmp1 = ((x * tmp0) >> 14)
    val tmp2 = ((24576 * tmp1) >> 14)
    val tmp3 = ((x * x) >> 14)
    val tmp4 = ((-24576 * tmp3) >> 14)
    val tmp5 = (tmp2 + (tmp4 << 1))
    val tmp6 = ((tmp5 * 21837) >> 15)
    val tmp7 = ((16384 * 21837) >> 14)
    val tmp8 = (tmp6 + tmp7)
    return tmp8
  }
  def bspline1_2D(x: Double): Double = ((((3.0 * (x * (x * x))) + (-6.0 * (x * x))) + 4.0) * 0.1666)
  def bspline1_2I(x: Int): Int = {
    val tmp0 = ((x * x) >> 14)
    val tmp1 = ((x * tmp0) >> 14)
    val tmp2 = ((24576 * tmp1) >> 14)
    val tmp3 = ((x * x) >> 14)
    val tmp4 = ((-24576 * tmp3) >> 14)
    val tmp5 = (tmp2 + (tmp4 << 1))
    val tmp6 = ((tmp5 + (16384 << 1)) >> 1)
    val tmp7 = ((tmp6 * 21837) >> 14)
    return tmp7
  }
  def bspline1_3D(x: Double): Double = ((4.0 + (((3.0 * x) + -6.0) * (x * x))) * 0.1666)
  def bspline1_3I(x: Int): Int = {
    val tmp0 = ((24576 * x) >> 14)
    val tmp1 = ((tmp0 + (-24576 << 1)) >> 1)
    val tmp2 = ((x * x) >> 14)
    val tmp3 = ((tmp1 * tmp2) >> 13)
    val tmp4 = (((16384 << 1) + tmp3) >> 1)
    val tmp5 = ((tmp4 * 21837) >> 14)
    return tmp5
  }
  def bspline1_4D(x: Double): Double = ((4.0 + (((-6.0 * x) + (x * (3.0 * x))) * x)) * 0.1666)
  def bspline1_4I(x: Int): Int = {
    val tmp0 = ((-24576 * x) >> 14)
    val tmp1 = ((24576 * x) >> 14)
    val tmp2 = ((x * tmp1) >> 14)
    val tmp3 = ((tmp0 << 1) + tmp2)
    val tmp4 = ((tmp3 * x) >> 14)
    val tmp5 = (((16384 << 1) + tmp4) >> 1)
    val tmp6 = ((tmp5 * 21837) >> 14)
    return tmp6
  }

  def bspline2_1D(x: Double): Double = (0.1666 * (((-3.0 * (x * (x * x))) + ((3.0 * (x * x)) + (3.0 * x))) + 1.0))
  def bspline2_1I(x: Int): Int = {
    val tmp0 = ((x * x) >> 14)
    val tmp1 = ((x * tmp0) >> 14)
    val tmp2 = ((-24576 * tmp1) >> 14)
    val tmp3 = ((x * x) >> 14)
    val tmp4 = ((24576 * tmp3) >> 14)
    val tmp5 = ((24576 * x) >> 14)
    val tmp6 = ((tmp4 + tmp5) >> 1)
    val tmp7 = (tmp2 + (tmp6 << 1))
    val tmp8 = (((tmp7 << 1) + 16384) >> 2)
    val tmp9 = ((21837 * tmp8) >> 14)
    return tmp9
  }
  def bspline2_2D(x: Double): Double = (((((-3.0 * (x * (x * x))) + (3.0 * (x * x))) + (3.0 * x)) + 1.0) * 0.1666)
  def bspline2_2I(x: Int): Int = {
    val tmp0 = ((x * x) >> 14)
    val tmp1 = ((x * tmp0) >> 14)
    val tmp2 = ((-24576 * tmp1) >> 14)
    val tmp3 = ((x * x) >> 14)
    val tmp4 = ((24576 * tmp3) >> 14)
    val tmp5 = ((tmp2 + tmp4) << 2)
    val tmp6 = ((24576 * x) >> 14)
    val tmp7 = ((tmp5 + (tmp6 << 2)) >> 2)
    val tmp8 = (((tmp7 << 1) + 16384) >> 2)
    val tmp9 = ((tmp8 * 21837) >> 14)
    return tmp9
  }
  def bspline2_3D(x: Double): Double = (((3.0 * x) + (1.0 + (x * (x + (x * (x * -3.0)))))) * 0.1666)
  def bspline2_3I(x: Int): Int = {
    val tmp0 = ((24576 * x) >> 14)
    val tmp1 = ((x * -24576) >> 14)
    val tmp2 = ((x * tmp1) >> 14)
    val tmp3 = ((x + (tmp2 << 1)) >> 1)
    val tmp4 = ((x * tmp3) >> 14)
    val tmp5 = (16384 + (tmp4 << 1))
    val tmp6 = (((tmp0 << 1) + tmp5) >> 1)
    val tmp7 = ((tmp6 * 21837) >> 15)
    return tmp7
  }
  def bspline2_4D(x: Double): Double = (0.1666 * (1.0 + (x * ((x + (-3.0 * (x * x))) + 3.0))))
  def bspline2_4I(x: Int): Int = {
    val tmp0 = ((x * x) >> 14)
    val tmp1 = ((-24576 * tmp0) >> 14)
    val tmp2 = ((x + (tmp1 << 1)) >> 1)
    val tmp3 = (tmp2 + 24576)
    val tmp4 = ((x * tmp3) >> 13)
    val tmp5 = ((16384 + tmp4) >> 1)
    val tmp6 = ((21837 * tmp5) >> 15)
    return tmp6
  }
  def bspline2_5D(x: Double): Double = ((1.0 + ((3.0 + (((x * -3.0) * x) + x)) * x)) * 0.1666)
  def bspline2_5I(x: Int): Int = {
    val tmp0 = ((x * -24576) >> 14)
    val tmp1 = ((tmp0 * x) >> 14)
    val tmp2 = (((tmp1 << 1) + x) >> 1)
    val tmp3 = (24576 + tmp2)
    val tmp4 = ((tmp3 * x) >> 13)
    val tmp5 = ((16384 + tmp4) >> 1)
    val tmp6 = ((tmp5 * 21837) >> 15)
    return tmp6
  }

  def field_dc_motor_1D(i_f: Double, ia: Double, omega: Double): Double = {
    val theta, a, b, c, rho = 1.0;  val epsilon = 0.1
    (1/((epsilon + (theta * ia))) * (((theta * ((a + b) * (i_f * ia))) +
      (theta * (rho * i_f))) + -(c * (((i_f * i_f) * omega) * theta))))
  }
  def field_dc_motor_1I(i_f: Int, ia: Int, omega: Int): Int = {
    val theta, a, b, c, rho = 16384; val epsilon = 26214
    val tmp0 = ((theta * ia) >> 14)
    val tmp1 = ((epsilon + (tmp0 << 4)) >> 4)
    val tmp2 = ((16384 << 13) / tmp1)
    val tmp3 = ((a + b) >> 1)
    val tmp4 = ((i_f * ia) >> 15)
    val tmp5 = ((tmp3 * tmp4) >> 14)
    val tmp6 = ((theta * tmp5) >> 14)
    val tmp7 = ((rho * i_f) >> 14)
    val tmp8 = ((theta * tmp7) >> 14)
    val tmp9 = (((tmp6 << 2) + tmp8) >> 2)
    val tmp10 = ((i_f * i_f) >> 15)
    val tmp11 = ((tmp10 * omega) >> 14)
    val tmp12 = ((tmp11 * theta) >> 14)
    val tmp13 = ((c * tmp12) >> 14)
    val tmp14 = -(tmp13)
    val tmp15 = (((tmp9 << 1) + tmp14) >> 1)
    val tmp16 = ((tmp2 * tmp15) >> 15)
    return tmp16 //10
  }

  def field_dc_motor_2D(i_f: Double, ia: Double, omega: Double): Double = {
    val theta, a, b, c, rho = 1.0;  val epsilon = 0.1
    (1/((epsilon + (theta * ia))) * ((theta * (((a + b) * (i_f * ia)) +
      (rho * i_f))) + -(c * (((i_f * i_f) * omega) * theta))))
  }
  def field_dc_motor_2I(i_f: Int, ia: Int, omega: Int): Int = {
    val theta, a, b, c, rho = 16384; val epsilon = 26214
    val tmp0 = ((theta * ia) >> 14)
    val tmp1 = ((epsilon + (tmp0 << 4)) >> 4)
    val tmp2 = ((16384 << 13) / tmp1)
    val tmp3 = ((a + b) >> 1)
    val tmp4 = ((i_f * ia) >> 15)
    val tmp5 = ((tmp3 * tmp4) >> 14)
    val tmp6 = ((rho * i_f) >> 14)
    val tmp7 = (((tmp5 << 2) + tmp6) >> 2)
    val tmp8 = ((theta * tmp7) >> 14)
    val tmp9 = ((i_f * i_f) >> 15)
    val tmp10 = ((tmp9 * omega) >> 14)
    val tmp11 = ((tmp10 * theta) >> 14)
    val tmp12 = ((c * tmp11) >> 14)
    val tmp13 = -(tmp12)
    val tmp14 = (((tmp8 << 1) + tmp13) >> 1)
    val tmp15 = ((tmp2 * tmp14) >> 15)
    return tmp15 //10
  }

  def field_dc_motor_3D(i_f: Double, ia: Double, omega: Double): Double = {
    val theta, a, b, c, rho = 1.0;  val epsilon = 0.1
    (1/((epsilon + (theta * ia))) * ((theta * ((((b + a) * i_f) * ia) +
      (rho * i_f))) + -(c * (((i_f * i_f) * omega) * theta))))
  }
  def field_dc_motor_3I(i_f: Int, ia: Int, omega: Int): Int = {
    val theta, a, b, c, rho = 16384; val epsilon = 26214
    val tmp0 = ((theta * ia) >> 14)
    val tmp1 = ((epsilon + (tmp0 << 4)) >> 4)
    val tmp2 = ((16384 << 13) / tmp1)
    val tmp3 = ((b + a) >> 1)
    val tmp4 = ((tmp3 * i_f) >> 14)
    val tmp5 = ((tmp4 * ia) >> 15)
    val tmp6 = ((rho * i_f) >> 14)
    val tmp7 = (((tmp5 << 2) + tmp6) >> 2)
    val tmp8 = ((theta * tmp7) >> 14)
    val tmp9 = ((i_f * i_f) >> 15)
    val tmp10 = ((tmp9 * omega) >> 14)
    val tmp11 = ((tmp10 * theta) >> 14)
    val tmp12 = ((c * tmp11) >> 14)
    val tmp13 = -(tmp12)
    val tmp14 = (((tmp8 << 1) + tmp13) >> 1)
    val tmp15 = ((tmp2 * tmp14) >> 15)
    return tmp15 //10
  }
   def field_dc_motor_4D(i_f: Double, ia: Double, omega: Double): Double = {
    val theta, a, b, c, rho = 1.0;  val epsilon = 0.1
    ((1/((epsilon + (theta * ia))) * (theta * (((b + a) * (ia * i_f)) +
      (rho * i_f)))) + (1/((epsilon + (theta * ia))) * -(c * ((i_f * i_f) * (omega * theta)))))
  }
  def field_dc_motor_4I(i_f: Int, ia: Int, omega: Int): Int = {
    val theta, a, b, c, rho = 16384; val epsilon = 26214
    val tmp0 = ((theta * ia) >> 14)
    val tmp1 = ((epsilon + (tmp0 << 4)) >> 4)
    val tmp2 = ((16384 << 13) / tmp1)
    val tmp3 = ((b + a) >> 1)
    val tmp4 = ((ia * i_f) >> 15)
    val tmp5 = ((tmp3 * tmp4) >> 14)
    val tmp6 = ((rho * i_f) >> 14)
    val tmp7 = (((tmp5 << 2) + tmp6) >> 2)
    val tmp8 = ((theta * tmp7) >> 14)
    val tmp9 = ((tmp2 * tmp8) >> 15)
    val tmp10 = ((theta * ia) >> 14)
    val tmp11 = ((epsilon + (tmp10 << 4)) >> 4)
    val tmp12 = ((16384 << 13) / tmp11)
    val tmp13 = ((i_f * i_f) >> 15)
    val tmp14 = ((omega * theta) >> 14)
    val tmp15 = ((tmp13 * tmp14) >> 14)
    val tmp16 = ((c * tmp15) >> 14)
    val tmp17 = -(tmp16)
    val tmp18 = ((tmp12 * tmp17) >> 15)
    val tmp19 = (((tmp9 << 1) + tmp18) >> 1)
    return tmp19 //10
  }

  def field_dc_motor_5D(i_f: Double, ia: Double, omega: Double): Double = {
    val theta, a, b, c, rho = 1.0;  val epsilon = 0.1
    (1/(((theta * ia) + epsilon)) * ((theta * ((((b * i_f) + (i_f * a)) * ia) +
      (rho * i_f))) + -(c * (((i_f * i_f) * omega) * theta))))
  }
  def field_dc_motor_5I(i_f: Int, ia: Int, omega: Int): Int = {
    val theta, a, b, c, rho = 16384; val epsilon = 26214
    val tmp0 = ((theta * ia) >> 14)
    val tmp1 = (((tmp0 << 4) + epsilon) >> 4)
    val tmp2 = ((16384 << 13) / tmp1)
    val tmp3 = ((b * i_f) >> 14)
    val tmp4 = ((i_f * a) >> 14)
    val tmp5 = ((tmp3 + tmp4) >> 1)
    val tmp6 = ((tmp5 * ia) >> 15)
    val tmp7 = ((rho * i_f) >> 14)
    val tmp8 = (((tmp6 << 2) + tmp7) >> 2)
    val tmp9 = ((theta * tmp8) >> 14)
    val tmp10 = ((i_f * i_f) >> 15)
    val tmp11 = ((tmp10 * omega) >> 14)
    val tmp12 = ((tmp11 * theta) >> 14)
    val tmp13 = ((c * tmp12) >> 14)
    val tmp14 = -(tmp13)
    val tmp15 = (((tmp9 << 1) + tmp14) >> 1)
    val tmp16 = ((tmp2 * tmp15) >> 15)
    return tmp16 //10
  }

  def hermite_1D(x: Double): Double =(((((((((((x * x) * x) * x) * x) * x) * x) * x) + (-28.0 * (((((x * x) * x) * x) * x) * x))) + (210.0 * (((x * x) * x) * x))) + (-420.0 * (x * x))) + 105.0)
  def hermite_1I(x: Long): Long = {
    val tmp0 = ((x * x) >> 30)
    val tmp1 = ((tmp0 * x) >> 30)
    val tmp2 = ((tmp1 * x) >> 31)
    val tmp3 = ((tmp2 * x) >> 30)
    val tmp4 = ((tmp3 * x) >> 30)
    val tmp5 = ((tmp4 * x) >> 31)
    val tmp6 = ((tmp5 * x) >> 30)
    val tmp7 = ((x * x) >> 30)
    val tmp8 = ((tmp7 * x) >> 30)
    val tmp9 = ((tmp8 * x) >> 31)
    val tmp10 = ((tmp9 * x) >> 30)
    val tmp11 = ((tmp10 * x) >> 30)
    val tmp12 = ((-1879048192L * tmp11) >> 31)
    val tmp13 = (((tmp6 << 2) + tmp12) >> 2)
    val tmp14 = ((x * x) >> 30)
    val tmp15 = ((tmp14 * x) >> 30)
    val tmp16 = ((tmp15 * x) >> 31)
    val tmp17 = ((1761607680L * tmp16) >> 31)
    val tmp18 = (((tmp13 << 5) + tmp17) >> 5)
    val tmp19 = ((x * x) >> 30)
    val tmp20 = ((-1761607680L * tmp19) >> 31)
    val tmp21 = (((tmp18 << 11) + tmp20) >> 11)
    val tmp22 = (((tmp21 << 20) + 1761607680L) >> 20)
    return tmp22
  }
  def hermite_2D(x: Double): Double = ((((((((((x * x) * x) * x) * x) * (x * x)) * x) + (-28.0 * (((((x * x) * x) * x) * x) * x))) + (210.0 * (((x * x) * x) * x))) + (-420.0 * (x * x))) + 105.0)
  def hermite_2I(x: Long): Long = {
    val tmp0 = ((x * x) >> 30)
    val tmp1 = ((tmp0 * x) >> 30)
    val tmp2 = ((tmp1 * x) >> 31)
    val tmp3 = ((tmp2 * x) >> 30)
    val tmp4 = ((x * x) >> 30)
    val tmp5 = ((tmp3 * tmp4) >> 31)
    val tmp6 = ((tmp5 * x) >> 30)
    val tmp7 = ((x * x) >> 30)
    val tmp8 = ((tmp7 * x) >> 30)
    val tmp9 = ((tmp8 * x) >> 31)
    val tmp10 = ((tmp9 * x) >> 30)
    val tmp11 = ((tmp10 * x) >> 30)
    val tmp12 = ((-1879048192L * tmp11) >> 31)
    val tmp13 = (((tmp6 << 2) + tmp12) >> 2)
    val tmp14 = ((x * x) >> 30)
    val tmp15 = ((tmp14 * x) >> 30)
    val tmp16 = ((tmp15 * x) >> 31)
    val tmp17 = ((1761607680L * tmp16) >> 31)
    val tmp18 = (((tmp13 << 5) + tmp17) >> 5)
    val tmp19 = ((x * x) >> 30)
    val tmp20 = ((-1761607680L * tmp19) >> 31)
    val tmp21 = (((tmp18 << 11) + tmp20) >> 11)
    val tmp22 = (((tmp21 << 20) + 1761607680L) >> 20)
    return tmp22
  }
  def hermite_3D(x: Double): Double = ((((((((((x * x) * x) * x) * x) * x) * (x * x)) + (-28.0 * (((((x * x) * x) * x) * x) * x))) + (210.0 * (((x * x) * x) * x))) + (-420.0 * (x * x))) + 105.0)
  def hermite_3I(x: Long): Long = {
    val tmp0 = ((x * x) >> 30)
    val tmp1 = ((tmp0 * x) >> 30)
    val tmp2 = ((tmp1 * x) >> 31)
    val tmp3 = ((tmp2 * x) >> 30)
    val tmp4 = ((tmp3 * x) >> 30)
    val tmp5 = ((x * x) >> 30)
    val tmp6 = ((tmp4 * tmp5) >> 31)
    val tmp7 = ((x * x) >> 30)
    val tmp8 = ((tmp7 * x) >> 30)
    val tmp9 = ((tmp8 * x) >> 31)
    val tmp10 = ((tmp9 * x) >> 30)
    val tmp11 = ((tmp10 * x) >> 30)
    val tmp12 = ((-1879048192L * tmp11) >> 31)
    val tmp13 = (((tmp6 << 2) + tmp12) >> 2)
    val tmp14 = ((x * x) >> 30)
    val tmp15 = ((tmp14 * x) >> 30)
    val tmp16 = ((tmp15 * x) >> 31)
    val tmp17 = ((1761607680L * tmp16) >> 31)
    val tmp18 = (((tmp13 << 5) + tmp17) >> 5)
    val tmp19 = ((x * x) >> 30)
    val tmp20 = ((-1761607680L * tmp19) >> 31)
    val tmp21 = (((tmp18 << 11) + tmp20) >> 11)
    val tmp22 = (((tmp21 << 20) + 1761607680L) >> 20)
    return tmp22
  }
  def hermite_4D(x: Double): Double = ((((((((((x * x) * x) * x) * x) * x) * x) * x) + ((-28.0 * (((((x * x) * x) * x) * x) * x)) + (210.0 * (((x * x) * x) * x)))) + (-420.0 * (x * x))) + 105.0)
  def hermite_4I(x: Long): Long = {
    val tmp0 = ((x * x) >> 30)
    val tmp1 = ((tmp0 * x) >> 30)
    val tmp2 = ((tmp1 * x) >> 31)
    val tmp3 = ((tmp2 * x) >> 30)
    val tmp4 = ((tmp3 * x) >> 30)
    val tmp5 = ((tmp4 * x) >> 31)
    val tmp6 = ((tmp5 * x) >> 30)
    val tmp7 = ((x * x) >> 30)
    val tmp8 = ((tmp7 * x) >> 30)
    val tmp9 = ((tmp8 * x) >> 31)
    val tmp10 = ((tmp9 * x) >> 30)
    val tmp11 = ((tmp10 * x) >> 30)
    val tmp12 = ((-1879048192L * tmp11) >> 31)
    val tmp13 = ((x * x) >> 30)
    val tmp14 = ((tmp13 * x) >> 30)
    val tmp15 = ((tmp14 * x) >> 31)
    val tmp16 = ((1761607680L * tmp15) >> 31)
    val tmp17 = (((tmp12 << 3) + tmp16) >> 3)
    val tmp18 = (((tmp6 << 2) + tmp17) >> 2)
    val tmp19 = ((x * x) >> 30)
    val tmp20 = ((-1761607680L * tmp19) >> 31)
    val tmp21 = (((tmp18 << 11) + tmp20) >> 11)
    val tmp22 = (((tmp21 << 20) + 1761607680L) >> 20)
    return tmp22
  }
  def hermite_5D(x: Double): Double = (((((((((x * x) * (x * x)) * x) * x) * x) * x) + (-28.0 * (((((x * x) * x) * x) * x) * x))) + ((210.0 * (((x * x) * x) * x)) + (-420.0 * (x * x)))) + 105.0)
  def hermite_5I(x: Long): Long = {
    val tmp0 = ((x * x) >> 30)
    val tmp1 = ((x * x) >> 30)
    val tmp2 = ((tmp0 * tmp1) >> 31)
    val tmp3 = ((tmp2 * x) >> 30)
    val tmp4 = ((tmp3 * x) >> 30)
    val tmp5 = ((tmp4 * x) >> 31)
    val tmp6 = ((tmp5 * x) >> 30)
    val tmp7 = ((x * x) >> 30)
    val tmp8 = ((tmp7 * x) >> 30)
    val tmp9 = ((tmp8 * x) >> 31)
    val tmp10 = ((tmp9 * x) >> 30)
    val tmp11 = ((tmp10 * x) >> 30)
    val tmp12 = ((-1879048192L * tmp11) >> 31)
    val tmp13 = (((tmp6 << 2) + tmp12) >> 2)
    val tmp14 = ((x * x) >> 30)
    val tmp15 = ((tmp14 * x) >> 30)
    val tmp16 = ((tmp15 * x) >> 31)
    val tmp17 = ((1761607680L * tmp16) >> 31)
    val tmp18 = ((x * x) >> 30)
    val tmp19 = ((-1761607680L * tmp18) >> 31)
    val tmp20 = (((tmp17 << 6) + tmp19) >> 5)
    val tmp21 = (((tmp13 << 6) + tmp20) >> 6)
    val tmp22 = (((tmp21 << 20) + 1761607680L) >> 20)
    return tmp22
  }
  def hermite_6D(x: Double): Double = ((((((x * ((((x * x) * x) * x) * x)) * x) * x) + (-28.0 * (((((x * x) * x) * x) * x) * x))) + ((((x * x) * x) * (x * 210.0)) + (-420.0 * (x * x)))) + 105.0)
  def hermite_6I(x: Long): Long = {
    val tmp0 = ((x * x) >> 30)
    val tmp1 = ((tmp0 * x) >> 30)
    val tmp2 = ((tmp1 * x) >> 31)
    val tmp3 = ((tmp2 * x) >> 30)
    val tmp4 = ((x * tmp3) >> 30)
    val tmp5 = ((tmp4 * x) >> 31)
    val tmp6 = ((tmp5 * x) >> 30)
    val tmp7 = ((x * x) >> 30)
    val tmp8 = ((tmp7 * x) >> 30)
    val tmp9 = ((tmp8 * x) >> 31)
    val tmp10 = ((tmp9 * x) >> 30)
    val tmp11 = ((tmp10 * x) >> 30)
    val tmp12 = ((-1879048192L * tmp11) >> 31)
    val tmp13 = (((tmp6 << 2) + tmp12) >> 2)
    val tmp14 = ((x * x) >> 30)
    val tmp15 = ((tmp14 * x) >> 30)
    val tmp16 = ((x * 1761607680L) >> 31)
    val tmp17 = ((tmp15 * tmp16) >> 31)
    val tmp18 = ((x * x) >> 30)
    val tmp19 = ((-1761607680L * tmp18) >> 31)
    val tmp20 = (((tmp17 << 6) + tmp19) >> 5)
    val tmp21 = (((tmp13 << 6) + tmp20) >> 6)
    val tmp22 = (((tmp21 << 20) + 1761607680L) >> 20)
    return tmp22
  }
  def hermite_7D(x: Double): Double = ((((((((x * x) * x) * x) * x) * x) * (x * x)) + (-28.0 * (((((x * x) * x) * x) * x) * x))) + (((-420.0 * (x * x)) + (210.0 * ((x * x) * (x * x)))) + 105.0))
  def hermite_7I(x: Long): Long = {
    val tmp0 = ((x * x) >> 30)
    val tmp1 = ((tmp0 * x) >> 30)
    val tmp2 = ((tmp1 * x) >> 31)
    val tmp3 = ((tmp2 * x) >> 30)
    val tmp4 = ((tmp3 * x) >> 30)
    val tmp5 = ((x * x) >> 30)
    val tmp6 = ((tmp4 * tmp5) >> 31)
    val tmp7 = ((x * x) >> 30)
    val tmp8 = ((tmp7 * x) >> 30)
    val tmp9 = ((tmp8 * x) >> 31)
    val tmp10 = ((tmp9 * x) >> 30)
    val tmp11 = ((tmp10 * x) >> 30)
    val tmp12 = ((-1879048192L * tmp11) >> 31)
    val tmp13 = (((tmp6 << 2) + tmp12) >> 2)
    val tmp14 = ((x * x) >> 30)
    val tmp15 = ((-1761607680L * tmp14) >> 31)
    val tmp16 = ((x * x) >> 30)
    val tmp17 = ((x * x) >> 30)
    val tmp18 = ((tmp16 * tmp17) >> 31)
    val tmp19 = ((1761607680L * tmp18) >> 31)
    val tmp20 = ((tmp15 + (tmp19 << 6)) >> 5)
    val tmp21 = (((tmp20 << 14) + 1761607680L) >> 14)
    val tmp22 = (((tmp13 << 6) + tmp21) >> 6)
    return tmp22
  }
  def hermite_8D(x: Double): Double = (((((((x * (x * x)) * x) * x) * x) * (x * x)) + (-28.0 * (((((x * x) * x) * x) * x) * x))) + (((-420.0 * (x * x)) + (210.0 * ((x * (x * x)) * x))) + 105.0))
  def hermite_8I(x: Long): Long = {
    val tmp0 = ((x * x) >> 30)
    val tmp1 = ((x * tmp0) >> 30)
    val tmp2 = ((tmp1 * x) >> 31)
    val tmp3 = ((tmp2 * x) >> 30)
    val tmp4 = ((tmp3 * x) >> 30)
    val tmp5 = ((x * x) >> 30)
    val tmp6 = ((tmp4 * tmp5) >> 31)
    val tmp7 = ((x * x) >> 30)
    val tmp8 = ((tmp7 * x) >> 30)
    val tmp9 = ((tmp8 * x) >> 31)
    val tmp10 = ((tmp9 * x) >> 30)
    val tmp11 = ((tmp10 * x) >> 30)
    val tmp12 = ((-1879048192L * tmp11) >> 31)
    val tmp13 = (((tmp6 << 2) + tmp12) >> 2)
    val tmp14 = ((x * x) >> 30)
    val tmp15 = ((-1761607680L * tmp14) >> 31)
    val tmp16 = ((x * x) >> 30)
    val tmp17 = ((x * tmp16) >> 30)
    val tmp18 = ((tmp17 * x) >> 31)
    val tmp19 = ((1761607680L * tmp18) >> 31)
    val tmp20 = ((tmp15 + (tmp19 << 6)) >> 5)
    val tmp21 = (((tmp20 << 14) + 1761607680L) >> 14)
    val tmp22 = (((tmp13 << 6) + tmp21) >> 6)
    return tmp22
  }
  def hermite_9D(x: Double): Double = ((((((((x * x) * x) * x) * x) * x) * (x * x)) + (-28.0 * (((((x * x) * x) * x) * x) * x))) + (((-420.0 * (x * x)) + (210.0 * (((x * x) * x) * x))) + 105.0))
  def hermite_9I(x: Long): Long = {
    val tmp0 = ((x * x) >> 30)
    val tmp1 = ((tmp0 * x) >> 30)
    val tmp2 = ((tmp1 * x) >> 31)
    val tmp3 = ((tmp2 * x) >> 30)
    val tmp4 = ((tmp3 * x) >> 30)
    val tmp5 = ((x * x) >> 30)
    val tmp6 = ((tmp4 * tmp5) >> 31)
    val tmp7 = ((x * x) >> 30)
    val tmp8 = ((tmp7 * x) >> 30)
    val tmp9 = ((tmp8 * x) >> 31)
    val tmp10 = ((tmp9 * x) >> 30)
    val tmp11 = ((tmp10 * x) >> 30)
    val tmp12 = ((-1879048192L * tmp11) >> 31)
    val tmp13 = (((tmp6 << 2) + tmp12) >> 2)
    val tmp14 = ((x * x) >> 30)
    val tmp15 = ((-1761607680L * tmp14) >> 31)
    val tmp16 = ((x * x) >> 30)
    val tmp17 = ((tmp16 * x) >> 30)
    val tmp18 = ((tmp17 * x) >> 31)
    val tmp19 = ((1761607680L * tmp18) >> 31)
    val tmp20 = ((tmp15 + (tmp19 << 6)) >> 5)
    val tmp21 = (((tmp20 << 14) + 1761607680L) >> 14)
    val tmp22 = (((tmp13 << 6) + tmp21) >> 6)
    return tmp22
  }


  def multi_poly_1D(a: Double, b: Double, c: Double) = ((((c + (a * b)) + 10.0) * (((a * c) + b) + 30.0)) * (((b * c) + a) + 20.0))
  def multi_poly_1I(a: Long, b: Long, c: Long): Long = {
    val tmp0 = ((a * b) >> 30)
    val tmp1 = ((c + (tmp0 << 3)) >> 3)
    val tmp2 = (((tmp1 << 3) + 1342177280L) >> 3)
    val tmp3 = ((a * c) >> 30)
    val tmp4 = (((tmp3 << 3) + b) >> 4)
    val tmp5 = (((tmp4 << 3) + 2013265920L) >> 3)
    val tmp6 = ((tmp2 * tmp5) >> 30)
    val tmp7 = ((b * c) >> 30)
    val tmp8 = (((tmp7 << 3) + a) >> 3)
    val tmp9 = (((tmp8 << 2) + 1342177280L) >> 2)
    val tmp10 = ((tmp6 * tmp9) >> 31)
    return tmp10
  }
  def multi_poly_2D(a: Double, b: Double, c: Double) = (((c + (a * b)) + 10.0) * ((((a * c) + b) + 30.0) * (((b * c) + a) + 20.0)))
  def multi_poly_2I(a: Long, b: Long, c: Long): Long = {
    val tmp0 = ((a * b) >> 30)
    val tmp1 = ((c + (tmp0 << 3)) >> 3)
    val tmp2 = (((tmp1 << 3) + 1342177280L) >> 3)
    val tmp3 = ((a * c) >> 30)
    val tmp4 = (((tmp3 << 3) + b) >> 4)
    val tmp5 = (((tmp4 << 3) + 2013265920L) >> 3)
    val tmp6 = ((b * c) >> 30)
    val tmp7 = (((tmp6 << 3) + a) >> 3)
    val tmp8 = (((tmp7 << 2) + 1342177280L) >> 2)
    val tmp9 = ((tmp5 * tmp8) >> 31)
    val tmp10 = ((tmp2 * tmp9) >> 30)
    return tmp10
  }
  def multi_poly_3D(a: Double, b: Double, c: Double) = (((((a * b) + c) + 10.0) * (((a * c) + b) + 30.0)) * ((b * c) + (a + 20.0)))
  def multi_poly_3I(a: Long, b: Long, c: Long): Long = {
    val tmp0 = ((a * b) >> 30)
    val tmp1 = (((tmp0 << 3) + c) >> 3)
    val tmp2 = (((tmp1 << 3) + 1342177280L) >> 3)
    val tmp3 = ((a * c) >> 30)
    val tmp4 = (((tmp3 << 3) + b) >> 4)
    val tmp5 = (((tmp4 << 3) + 2013265920L) >> 3)
    val tmp6 = ((tmp2 * tmp5) >> 30)
    val tmp7 = ((b * c) >> 30)
    val tmp8 = ((a + (1342177280L << 1)) >> 1)
    val tmp9 = (((tmp7 << 2) + tmp8) >> 2)
    val tmp10 = ((tmp6 * tmp9) >> 31)
    return tmp10
  }
  def multi_poly_4D(a: Double, b: Double, c: Double) = (((((c + (a * b)) + 10.0) * ((a * c) + b)) + (((c + (a * b)) + 10.0) * 30.0)) * ((b * c) + (a + 20.0)))
  def multi_poly_4I(a: Long, b: Long, c: Long): Long = {
    val tmp0 = ((a * b) >> 30)
    val tmp1 = ((c + (tmp0 << 3)) >> 3)
    val tmp2 = (((tmp1 << 3) + 1342177280L) >> 3)
    val tmp3 = ((a * c) >> 30)
    val tmp4 = (((tmp3 << 3) + b) >> 4)
    val tmp5 = ((tmp2 * tmp4) >> 30)
    val tmp6 = ((a * b) >> 30)
    val tmp7 = ((c + (tmp6 << 3)) >> 3)
    val tmp8 = (((tmp7 << 3) + 1342177280L) >> 3)
    val tmp9 = ((tmp8 * 2013265920L) >> 31)
    val tmp10 = (((tmp5 << 2) + tmp9) >> 2)
    val tmp11 = ((b * c) >> 30)
    val tmp12 = ((a + (1342177280L << 1)) >> 1)
    val tmp13 = (((tmp11 << 2) + tmp12) >> 2)
    val tmp14 = ((tmp10 * tmp13) >> 31)
    return tmp14
  }
  def multi_poly_5D(a: Double, b: Double, c: Double) = (((((a * b) + c) * (((a * c) + b) + 30.0)) + (10.0 * (((c * a) + b) + 30.0))) * ((b * c) + (a + 20.0)))
  def multi_poly_5I(a: Long, b: Long, c: Long): Long = {
    val tmp0 = ((a * b) >> 30)
    val tmp1 = (((tmp0 << 3) + c) >> 3)
    val tmp2 = ((a * c) >> 30)
    val tmp3 = (((tmp2 << 3) + b) >> 4)
    val tmp4 = (((tmp3 << 3) + 2013265920L) >> 3)
    val tmp5 = ((tmp1 * tmp4) >> 30)
    val tmp6 = ((c * a) >> 30)
    val tmp7 = (((tmp6 << 3) + b) >> 4)
    val tmp8 = (((tmp7 << 3) + 2013265920L) >> 3)
    val tmp9 = ((1342177280L * tmp8) >> 30)
    val tmp10 = (((tmp5 << 3) + tmp9) >> 3)
    val tmp11 = ((b * c) >> 30)
    val tmp12 = ((a + (1342177280L << 1)) >> 1)
    val tmp13 = (((tmp11 << 2) + tmp12) >> 2)
    val tmp14 = ((tmp10 * tmp13) >> 31)
    return tmp14
  }
  def multi_poly_6D(a: Double, b: Double, c: Double) = (((((c + (b * a)) + 10.0) * ((a * c) + b)) + (((b * a) + (c + 10.0)) * 30.0)) * ((c * b) + (20.0 + a)))
  def multi_poly_6I(a: Long, b: Long, c: Long): Long = {
    val tmp0 = ((b * a) >> 30)
    val tmp1 = ((c + (tmp0 << 3)) >> 3)
    val tmp2 = (((tmp1 << 3) + 1342177280L) >> 3)
    val tmp3 = ((a * c) >> 30)
    val tmp4 = (((tmp3 << 3) + b) >> 4)
    val tmp5 = ((tmp2 * tmp4) >> 30)
    val tmp6 = ((b * a) >> 30)
    val tmp7 = ((c + 1342177280L) >> 1)
    val tmp8 = (((tmp6 << 2) + tmp7) >> 2)
    val tmp9 = ((tmp8 * 2013265920L) >> 31)
    val tmp10 = (((tmp5 << 2) + tmp9) >> 2)
    val tmp11 = ((c * b) >> 30)
    val tmp12 = (((1342177280L << 1) + a) >> 1)
    val tmp13 = (((tmp11 << 2) + tmp12) >> 2)
    val tmp14 = ((tmp10 * tmp13) >> 31)
    return tmp14
  }
  def multi_poly_7D(a: Double, b: Double, c: Double) = ((((((c + (b * a)) + 10.0) * (a * c)) + (((c + (b * a)) + 10.0) * b)) + (30.0 * ((a * b) + (c + 10.0)))) * ((b * c) + (a + 20.0)))
  def multi_poly_7I(a: Long, b: Long, c: Long): Long = {
    val tmp0 = ((b * a) >> 30)
    val tmp1 = ((c + (tmp0 << 3)) >> 3)
    val tmp2 = (((tmp1 << 3) + 1342177280L) >> 3)
    val tmp3 = ((a * c) >> 30)
    val tmp4 = ((tmp2 * tmp3) >> 31)
    val tmp5 = ((b * a) >> 30)
    val tmp6 = ((c + (tmp5 << 3)) >> 3)
    val tmp7 = (((tmp6 << 3) + 1342177280L) >> 3)
    val tmp8 = ((tmp7 * b) >> 30)
    val tmp9 = (((tmp4 << 4) + tmp8) >> 4)
    val tmp10 = ((a * b) >> 30)
    val tmp11 = ((c + 1342177280L) >> 1)
    val tmp12 = (((tmp10 << 2) + tmp11) >> 2)
    val tmp13 = ((2013265920L * tmp12) >> 31)
    val tmp14 = (((tmp9 << 2) + tmp13) >> 2)
    val tmp15 = ((b * c) >> 30)
    val tmp16 = ((a + (1342177280L << 1)) >> 1)
    val tmp17 = (((tmp15 << 2) + tmp16) >> 2)
    val tmp18 = ((tmp14 * tmp17) >> 31)
    return tmp18
  }
  def multi_poly_8D(a: Double, b: Double, c: Double) = ((((((c + (b * a)) + 10.0) * (a * c)) + (b * ((c + (b * a)) + 10.0))) * ((b * c) + (a + 20.0))) + ((30.0 * ((a * b) + (c + 10.0))) * ((b * c) + (a + 20.0))))
  def multi_poly_8I(a: Long, b: Long, c: Long): Long = {
    val tmp0 = ((b * a) >> 30)
    val tmp1 = ((c + (tmp0 << 3)) >> 3)
    val tmp2 = (((tmp1 << 3) + 1342177280L) >> 3)
    val tmp3 = ((a * c) >> 30)
    val tmp4 = ((tmp2 * tmp3) >> 31)
    val tmp5 = ((b * a) >> 30)
    val tmp6 = ((c + (tmp5 << 3)) >> 3)
    val tmp7 = (((tmp6 << 3) + 1342177280L) >> 3)
    val tmp8 = ((b * tmp7) >> 30)
    val tmp9 = (((tmp4 << 4) + tmp8) >> 4)
    val tmp10 = ((b * c) >> 30)
    val tmp11 = ((a + (1342177280L << 1)) >> 1)
    val tmp12 = (((tmp10 << 2) + tmp11) >> 2)
    val tmp13 = ((tmp9 * tmp12) >> 31)
    val tmp14 = ((a * b) >> 30)
    val tmp15 = ((c + 1342177280L) >> 1)
    val tmp16 = (((tmp14 << 2) + tmp15) >> 2)
    val tmp17 = ((2013265920L * tmp16) >> 31)
    val tmp18 = ((b * c) >> 30)
    val tmp19 = ((a + (1342177280L << 1)) >> 1)
    val tmp20 = (((tmp18 << 2) + tmp19) >> 2)
    val tmp21 = ((tmp17 * tmp20) >> 31)
    val tmp22 = (((tmp13 << 2) + tmp21) >> 2)
    return tmp22
  }
  def multi_poly_9D(a: Double, b: Double, c: Double) = ((((((c + (b * a)) * a) + (10.0 * a)) * c) + ((((c + (b * a)) + 10.0) * b) + ((30.0 * (a * b)) + (30.0 * (c + 10.0))))) * ((c * b) + (a + 20.0)))
  def multi_poly_9I(a: Long, b: Long, c: Long): Long = {
    val tmp0 = ((b * a) >> 30)
    val tmp1 = ((c + (tmp0 << 3)) >> 3)
    val tmp2 = ((tmp1 * a) >> 30)
    val tmp3 = ((1342177280L * a) >> 30)
    val tmp4 = (((tmp2 << 3) + tmp3) >> 3)
    val tmp5 = ((tmp4 * c) >> 31)
    val tmp6 = ((b * a) >> 30)
    val tmp7 = ((c + (tmp6 << 3)) >> 3)
    val tmp8 = (((tmp7 << 3) + 1342177280L) >> 3)
    val tmp9 = ((tmp8 * b) >> 30)
    val tmp10 = ((a * b) >> 30)
    val tmp11 = ((2013265920L * tmp10) >> 31)
    val tmp12 = ((c + 1342177280L) >> 1)
    val tmp13 = ((2013265920L * tmp12) >> 31)
    val tmp14 = (((tmp11 << 2) + tmp13) >> 2)
    val tmp15 = ((tmp9 + (tmp14 << 2)) >> 2)
    val tmp16 = (((tmp5 << 2) + tmp15) >> 2)
    val tmp17 = ((c * b) >> 30)
    val tmp18 = ((a + (1342177280L << 1)) >> 1)
    val tmp19 = (((tmp17 << 2) + tmp18) >> 2)
    val tmp20 = ((tmp16 * tmp19) >> 31)
    return tmp20
  }


  def doppler_1D(t: Double, f: Double, u: Double): Double = {
    //println("t: " + t + "  f:" + f + "  u:" + u)
    val x1 = ((331.4 + (0.6 * t)) * f)
    val x2 = 1/((((331.4 + (0.6 * t)) + u) * ((331.4 + (0.6 * t)) + u)))
    val result = x1 * x2
    //println("x1 " + x1 + "    x2 " + x2 + "    result " + result)
    result
  }
  def doppler_1I(t: Long, f: Long, u: Long): Long = {
    val tmp0 = ((1288490189 * t) >> 30)
    val tmp1 = (((1389992346 << 4) + tmp0) >> 4)
    val tmp2 = ((tmp1 * f) >> 30)
    val tmp3 = ((tmp2 * 1073741824) >> 30)
    val tmp4 = ((1288490189 * t) >> 30)
    val tmp5 = (((1389992346 << 4) + tmp4) >> 4)
    val tmp6 = (((tmp5 << 2) + u) >> 2)
    val tmp7 = ((1288490189 * t) >> 30)
    val tmp8 = (((1389992346 << 4) + tmp7) >> 4)
    val tmp9 = (((tmp8 << 2) + u) >> 2)
    val tmp10 = ((tmp6 * tmp9) >> 31)
    //println("t: " + t + "  f:" + f + "  u:" + u)
    //println(tmp10)
    val tmp11 = ((tmp3 << 27) / tmp10)
    return tmp11 //22
  }
  def doppler_2D(t: Double, f: Double, u: Double): Double = {
    (((331.4 + (0.6 * t)) * f) * 1/(((331.4 + ((0.6 * t) + u)) * (331.4 + ((0.6 * t) + u)))))
  }
  def doppler_2I(t: Long, f: Long, u: Long): Long = {
    val tmp0 = ((1288490189 * t) >> 30)
    val tmp1 = (((1389992346 << 4) + tmp0) >> 4)
    val tmp2 = ((tmp1 * f) >> 30)
    val tmp3 = ((tmp2 * 1073741824) >> 30)
    val tmp4 = ((1288490189 * t) >> 30)
    val tmp5 = ((tmp4 + (u << 2)) >> 3)
    val tmp6 = (((1389992346 << 1) + tmp5) >> 1)
    val tmp7 = ((1288490189 * t) >> 30)
    val tmp8 = ((tmp7 + (u << 2)) >> 3)
    val tmp9 = (((1389992346 << 1) + tmp8) >> 1)
    val tmp10 = ((tmp6 * tmp9) >> 31)
    val tmp11 = ((tmp3 << 27) / tmp10)
    return tmp11 //22
  }
  def doppler_3D(t: Double, f: Double, u: Double): Double = {
    (((331.4 + (t * 0.6)) * f) * (1/(((331.4 + (t * 0.6)) + u)) * 1/(((331.4 + (t * 0.6)) + u))))
  }
  def doppler_3I(t: Long, f: Long, u: Long): Long = {
    val tmp0 = ((t * 1288490189) >> 30)
    val tmp1 = (((1389992346 << 4) + tmp0) >> 4)
    val tmp2 = ((tmp1 * f) >> 30)
    val tmp3 = ((t * 1288490189) >> 30)
    val tmp4 = (((1389992346 << 4) + tmp3) >> 4)
    val tmp5 = (((tmp4 << 2) + u) >> 2)
    val tmp6 = ((1073741824 << 23) / tmp5)
    val tmp7 = ((tmp6 * 1073741824) >> 30)
    val tmp8 = ((t * 1288490189) >> 30)
    val tmp9 = (((1389992346 << 4) + tmp8) >> 4)
    val tmp10 = (((tmp9 << 2) + u) >> 2)
    val tmp11 = ((tmp7 << 22) / tmp10)
    val tmp12 = ((tmp2 * tmp11) >> 16)
    return tmp12 //23
  }
  def doppler_4D(t: Double, f: Double, u: Double): Double = {
    ((1/(((331.4 + ((0.6 * t) + u)) * (331.4 + (u + (0.6 * t))))) * ((0.6 * t) + 331.4)) * f)
  }
  def doppler_4I(t: Long, f: Long, u: Long): Long = {
    val tmp0 = ((1288490189 * t) >> 30)
    val tmp1 = ((tmp0 + (u << 2)) >> 3)
    val tmp2 = (((1389992346 << 1) + tmp1) >> 1)
    val tmp3 = ((1288490189 * t) >> 30)
    val tmp4 = (((u << 2) + tmp3) >> 3)
    val tmp5 = (((1389992346 << 1) + tmp4) >> 1)
    val tmp6 = ((tmp2 * tmp5) >> 31)
    val tmp7 = ((1073741824 << 14) / tmp6)
    val tmp8 = ((1288490189 * t) >> 30)
    val tmp9 = ((tmp8 + (1389992346 << 4)) >> 4)
    val tmp10 = ((tmp7 * tmp9) >> 22)
    val tmp11 = ((tmp10 * f) >> 25)
    return tmp11 // 22
  }

}
