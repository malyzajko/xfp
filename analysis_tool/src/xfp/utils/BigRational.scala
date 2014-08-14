package xfp.utils

import scala.math.{ScalaNumericConversions, ScalaNumber}
import java.math.BigInteger

object BigRational {

  // Note, this only works for 'normal' doubles and not for scientific notation
  def getBigRationalFromDouble(d: Double): (Long, Long) = {
    var dString = d.toString.replace("e", "E")

    if (dString.contains("E")) {
      val pieces = dString.split("""E""")
      val (num, den) = getBigRationalFromDouble(pieces(0).toDouble)
      val tenMultiple = den.toString.length - 1 -pieces(1).toInt
      if (tenMultiple > 0)
        return (num, math.pow(10, den.toString.length - 1 -pieces(1).toInt).toLong)
      else
        return ((num * math.pow(10, -tenMultiple)).toLong, 0L)
    }
    else {
      val pieces = dString.split("""\.""")
      assert(pieces.length == 2)
      val num = (pieces(0) + pieces(1)).toLong
      val den = math.pow(10, pieces(1).length).toLong
      return (num, den)
    }
  }

  // Fixme: this is anything but sound...
  def apply(dbl: Double): BigRational = {
    val (n, d) = getBigRationalFromDouble(dbl)
    BigRational(n, d)
  }

  // convenience constructors only for small numbers
  def apply(n: Int): BigRational = new BigRational(new BigInt(new BigInteger(n.toString)), 1)
  def apply(n: Int, d: Int): BigRational = {
    val (num, den) = formatFraction(new BigInt(new BigInteger(n.toString)), new BigInt(new BigInteger(d.toString)))
    if(den < 0) new BigRational(-num, -den)
    else new BigRational(num, den)
  }

  def apply(n: BigInt, d: BigInt): BigRational = {
    val (num, den) = formatFraction(n, d)
    if(den < 0) new BigRational(-num, -den)
    else new BigRational(num, den)
  }

  def abs(r: BigRational): BigRational = {
    if (r.n < 0) return new BigRational(-r.n, r.d)
    else return r
  }

  def max(x: BigRational, y: BigRational): BigRational = {
    if (x >= y) return x
    else return y
  }

  def min(x: BigRational, y: BigRational): BigRational = {
    if (x < y) return x
    else return y
  }

  private def formatFraction(n: BigInt, d: BigInt): (BigInt, BigInt) = {
    val g = gcd(n.abs, d.abs)
    (n / g, d / g)
  }

  private def gcd(a: BigInt, b: BigInt): BigInt =
    if (b == 0) a else gcd(b, a % b)
}

class BigRational(val n: BigInt, val d: BigInt) extends ScalaNumber with ScalaNumericConversions
  with Ordered[BigRational] {
  import BigRational._
  assert(d > 0, "BigRational denominator negative! " + d)  // number can be negative only through nominator
  assert(math.abs(gcd(n, d).toLong) == 1, "BigRational not reduced %d / %d!".format(n, d))  // fraction is reduced

  def unary_-(): BigRational = BigRational(-n, d)
  def +(other: BigRational): BigRational = BigRational(n * other.d + other.n * d, d * other.d)
  def -(other: BigRational): BigRational = BigRational(n * other.d - other.n * d, d * other.d)
  def *(other: BigRational): BigRational = BigRational(n * other.n, d * other.d)
  def /(other: BigRational): BigRational = BigRational(n * other.d, d* other.n)

  override def toString: String = "%f".format(this.toDouble)

  def integerPart: Int = doubleValue.toInt

  def compare(other: BigRational): Int = {
    val xNom = this.n * other.d
    val yNom = other.n * this.d
    val denom = this.d * other.d
    return xNom.compare(yNom)
  }

  override def equals(other: Any): Boolean = other match {
    case x: BigRational => this.compare(x) == 0
    case x: Double => this.toDouble == x  // Fixme: not ideal
    case x: Short => this.compare(BigRational(x)) == 0
    case x: Char => this.compare(BigRational(x)) == 0
    case x: Byte => this.compare(BigRational(x)) == 0
    case x: Int => this.compare(BigRational(x)) == 0
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
