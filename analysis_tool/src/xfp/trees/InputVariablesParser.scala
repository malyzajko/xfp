package xfp.trees

import scala.util.parsing.combinator._
import xfp.fixedpoint.{Interval, FixedPointFormat}
import xfp.utils.{BigRational => Rational}

object MatlabInputVariablesParser {
  def parseFile(fileName: String): (Map[String, (FixedPointFormat, Interval)],
    Map[String, FixedPointFormat]) = {
    var formats = collection.immutable.HashMap[String, FixedPointFormat]()
    var inputs = collection.immutable.HashMap[String,(FixedPointFormat, Interval)]()

    val source = scala.io.Source.fromFile(fileName)
    val lines = source.mkString.split("\n")
    // We assume the following format
    // first line contains one number which is the bitwidth:
    val bitwidth = lines(0).replaceAll("""\s+$""", "").toInt

    for (line <- lines.tail) {
      val pieces =  line.split("""\s""")
      val name = pieces(0)
      val a = pieces(1).toDouble
      val b = pieces(2).toDouble
      val format = FixedPointFormat.getFormat(Rational(a), Rational(b), bitwidth)
      formats += ((name, format))
      inputs += ((name, (format, Interval(a, b))))
    }
    source.close()
    return (inputs, formats)
  }

}

object InputVariablesParser extends RegexParsers {

  val ident = """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*"""r
  val integer = """([-]*[1-9][0-9]*)|0"""r
  val float = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r

  def number = (float | integer)

  def inputDecl: Parser[(String, FixedPointFormat, Interval)] =
    ident ~ ident ~ integer ~ integer ~ integer ~ number ~ number ^^ {
      case n1 ~ n2 ~ sign ~ bits ~ frac ~ xlo ~ xhi =>
        val signed = if (sign == "1") true else false
        (n1, new FixedPointFormat(signed, bits.toInt, frac.toInt, false),
          Interval(xlo.toDouble, xhi.toDouble))
    }

  def formatDecl: Parser[(String, FixedPointFormat, Interval)] =
    ident ~ ident ~ integer ~ integer ~ integer ^^ {
      case n1 ~ n2 ~ sign ~ bits ~ frac =>
        val signed = if (sign == "1") true else false
        (n1, new FixedPointFormat(signed, bits.toInt, frac.toInt, false), null)
    }


  def inputLine: Parser[(String, FixedPointFormat, Interval)] =
    ( inputDecl | formatDecl )

  def parse(exprStr: String): (String, FixedPointFormat, Interval) = {
    parseAll(inputLine, exprStr) match {
      case Success(input, _) =>
        input
      case e: NoSuccess =>
        println(e)
        null
    }
  }

  def parseFile(fileName: String): (Map[String,(FixedPointFormat, Interval)],
     Map[String, FixedPointFormat]) = {

    var formats = collection.immutable.HashMap[String, FixedPointFormat]()
    var inputs = collection.immutable.HashMap[String,(FixedPointFormat, Interval)]()

    val source = scala.io.Source.fromFile(fileName)
    val lines = source.mkString.split("\n")
    for (line <- lines) {
      if (! (line.startsWith("Block") || line.startsWith("End"))) {
        val result = parse(line)
        if (result._3 == null)
          formats += ((result._1, result._2))
        else
          inputs += ((result._1, (result._2, result._3)))
      }
    }
    source.close()
    (inputs, formats)
  }

  // only works for when both format and range are given
  def parseFileJava(fileName: String): (java.util.Map[String, FixedPointFormat],
     java.util.Map[String, Interval]) = {

    var formats = new java.util.HashMap[String, FixedPointFormat]()
    var ranges = new java.util.HashMap[String, Interval]()

    val source = scala.io.Source.fromFile(fileName)
    val lines = source.mkString.split("\n")

    if (lines(0).startsWith("Block")) {

      for (line <- lines) {
        if (! (line.startsWith("Block") || line.startsWith("End"))) {
          val result = parse(line)
          formats.put(result._1, result._2)
          ranges.put(result._1, result._3)
        }
      }
    }
    else {
      // We assume the following format
      // first line contains one number which is the bitwidth:
      val bitwidth = lines(0).replaceAll("""\s+$""", "").toInt

      for (line <- lines.tail) {
        val pieces =  line.split("""\s""")
        val name = pieces(0)
        val a = pieces(1).toDouble
        val b = pieces(2).toDouble
        val format = FixedPointFormat.getFormat(Rational(a), Rational(b), bitwidth)
        formats.put(name, format)
        ranges.put(name, Interval(a, b))
      }
    }
    source.close()
    (formats, ranges)
  }

}
