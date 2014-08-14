package xfp.utils

import scala.math.{ScalaNumericConversions, ScalaNumber}

object Rational {

  // Note, this only works for 'normal' doubles and not for scientific notation
  def getRationalFromDouble(d: Double): (Long, Long) = {
    var dString = d.toString
    val pieces = dString.split("""\.""")
    assert(pieces.length == 2)
    val num = (pieces(0) + pieces(1)).toLong
    val den = math.pow(10, pieces(1).length).toLong
    return (num, den)
  }

  // Fixme: this is anything but sound...
  def apply(dbl: Double): Rational = {
    val (n, d) = getRationalFromDouble(dbl)
    Rational(n, d)
  }

  def apply(n: Long): Rational = new Rational(n, 1)
  def apply(n: Long, d: Long): Rational = {
    val (num, den) = formatFraction(n, d)
    if(den < 0) new Rational(-num, -den)
    else new Rational(num, den)
  }

  def abs(r: Rational): Rational = {
    if (r.n < 0) return new Rational(-r.n, r.d)
    else return r
  }

  def max(x: Rational, y: Rational): Rational = {
    if (x >= y) return x
    else return y
  }

  def min(x: Rational, y: Rational): Rational = {
    if (x < y) return x
    else return y
  }

  private def formatFraction(n: Long, d: Long): (Long, Long) = {
    val g = gcd(n.abs, d.abs)
    (n / g, d / g)
  }

  private def gcd(a: Long, b: Long): Long =
    if (b == 0) a else gcd(b, a % b)
}

class Rational(val n: Long, val d: Long) extends ScalaNumber with ScalaNumericConversions
  with Ordered[Rational] {
  import Rational._
  assert(d > 0, "Rational denominator negative!")  // number can be negative only through nominator
  assert(math.abs(gcd(n, d)) == 1, "Rational not reduced %d / %d!".format(n, d))  // fraction is reduced

  def unary_-(): Rational = Rational(-n, d)
  def +(other: Rational): Rational = Rational(n * other.d + other.n * d, d * other.d)
  def -(other: Rational): Rational = Rational(n * other.d - other.n * d, d * other.d)
  def *(other: Rational): Rational = Rational(n * other.n, d * other.d)
  def /(other: Rational): Rational = Rational(n * other.d, d* other.n)

  override def toString: String = n + "\\" + d + " ("+ this.toDouble + ")"

  def integerPart: Int = doubleValue.toInt

  def compare(other: Rational): Int = {
    val xNom = this.n * other.d
    val yNom = other.n * this.d
    val denom = this.d * other.d
    return xNom.compare(yNom)
  }

  override def equals(other: Any): Boolean = other match {
    case x: Rational => this.compare(x) == 0
    case x: Double => this.toDouble == x  // Fixme: not ideal
    case x: Short => this.compare(Rational(x)) == 0
    case x: Char => this.compare(Rational(x)) == 0
    case x: Byte => this.compare(Rational(x)) == 0
    case x: Int => this.compare(Rational(x)) == 0
    case x: Float => this.toFloat == x
    case x: Long => this.toLong == x
    case _ => false
  }

  override def byteValue(): Byte = Predef.double2Double(doubleValue).byteValue
  override def doubleValue(): Double = n.toDouble / d.toDouble
  override def floatValue(): Float = Predef.double2Double(doubleValue).floatValue
  override def intValue(): Int = Predef.double2Double(doubleValue).intValue
  override def isValidByte: Boolean = false
  override def isValidChar: Boolean = false
  override def isValidInt: Boolean = false
  override def isValidShort: Boolean = false
  override def longValue(): Long = Predef.double2Double(doubleValue).longValue
  override def shortValue(): Short = Predef.double2Double(doubleValue).shortValue
  override def toByte: Byte = doubleValue.toByte
  override def toChar: Char = doubleValue.toChar
  override def toDouble: Double = doubleValue
  override def toFloat: Float = doubleValue.toFloat
  override def toInt: Int = doubleValue.toInt
  override def toLong: Long = doubleValue.toLong
  override def toShort: Short = doubleValue.toShort
  def underlying(): AnyRef = this
  override def isWhole(): Boolean = d == 1.0
 }
