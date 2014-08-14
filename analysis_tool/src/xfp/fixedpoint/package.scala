package xfp

package object fixedpoint {
  case class UnsupportedOperationException(s: String) extends Exception
  case class IncompatibleFixedPointFormatsException(s: String) extends Exception
  case class FixedPointOverflowException(s: String) extends Exception
  case class DivisionByZeroException(s: String) extends Exception

  /** Threshold after which we do packing. */
  var maxNoiseCount = 200

  /** If true, will print a warning each time a comparison fails.*/
  var printComparisonFailure = false

  /** String that will be printed when a comparison fails, to help with debugging.*/
  var failMessage = ""

  /** Formatting to be used when printing doubles. */
  var doubleFormat = "%1.4e"

  // If set to false, we will not check for overflow.
  // Choose this option if you are sure about the formats only.
  var checkForOverflow = true

  /** If true, the errorMultiplier for division will be at most -100 */
  var divisionHack = false

  /** If true, multiplication by exact constants will not add new errors.*/
  var exactConstantMultiplication = true

  /** Print extra information. */
  var debugMode = false
}
