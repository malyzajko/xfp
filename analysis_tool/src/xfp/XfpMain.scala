
import xfp.trees._
import xfp.analysis.{FixedPointAnalyzer, IntervalDivision}
import xfp.synthesis.FixedPointCodeGenerator
import xfp.fixedpoint.{Interval, FixedPointFormat}
import collection.mutable.{HashMap => HMap, Map => MMap}
import collection.mutable.Queue
import scala.io.Source

import java.io.FileWriter

/** Front-end for the fixed-point code analysis tool.*/
object XfpMain {


  def main(args: Array[String]) {
    object InputMode extends Enumeration {
      type InputMode = Value
      val CCode, Expr, NoInput = Value
    }
    object OutputMode extends Enumeration {
      type OutputMode = Value
      val ScalaFP, CFP, Analysis, NoOutput = Value
    }
    import InputMode._
    import OutputMode._
    var inputMode = NoInput
    var outputMode = NoOutput
    var bits = 16
    var outputName = ""
    var rounding = false
    var programFile = ""
    var inputsFile = ""
    var printHelp = false
    var simulationCount = -1
    var multipleExpressions = false
    var callingFromMatlab = false

    // These are internal options
    xfp.fixedpoint.debugMode = false
    xfp.fixedpoint.divisionHack = true
    xfp.fixedpoint.exactConstantMultiplication = true

    def arguments(args: List[String]): Unit = args match {
      case Nil =>
      // Inputs
      case "-code" :: file :: rest => inputMode = CCode; programFile = file; arguments(rest)
      case "-expr" :: file :: rest => inputMode = Expr; programFile = file; arguments(rest)
      case "-inputs" :: file :: rest => inputsFile = file; arguments(rest)
      // What to do
      case "-a" :: rest => outputMode = Analysis; arguments(rest)
      case "-c" :: rest => outputMode = CFP; arguments(rest)
      case "-s" :: rest => outputMode = ScalaFP; arguments(rest)
      //case "-n" :: name :: rest => fncName = name; arguments(rest)
      case "-matlab" :: rest => callingFromMatlab = true; arguments(rest)
      case "-b" :: number :: rest => bits = number.toInt; arguments(rest)
      case "-sim" :: number :: rest => simulationCount = number.toInt; arguments(rest)
      case "-mult" :: rest => multipleExpressions = true; arguments(rest)
      case "-o" :: output :: rest => outputName = output; arguments(rest)
      case "-h" :: rest => printHelp = true
      case "-r" :: rest => rounding = true; arguments(rest)
      case _ => println("cannot parse option " + args); printHelp = true
    }

    arguments(args.toList)

    if (inputMode == NoInput || outputMode == NoOutput || programFile == "" || inputsFile == "" || printHelp) {
      printHelpOptions
      return
    }

    try {
      // inputs: Map[String,(FixedPointFormat, Interval)]
      // formats: Map[String, FixedPointFormat]
      val (inputs, formats) =
        if (callingFromMatlab)
          MatlabInputVariablesParser.parseFile(inputsFile)
        else
         InputVariablesParser.parseFile(inputsFile)

      // Check number of bits, maybe we forgot to change on command line
      if (inputs.head._2._1.bits != bits) {
        bits = inputs.head._2._1.bits
        if (xfp.fixedpoint.debugMode)
          println("Different number of bits detected, changing default to " + bits)
      }

      inputMode match {
        case CCode =>
          val ctree = CParser.parseFile(programFile)
          if (ctree == null) { println("Parse error."); return }
          if ( !FixedPointAnalyzer.analyzeAll(ctree, inputs, formats, bits, rounding))
            return
          //ctree
          outputMode match {
            case Analysis =>
              if (outputName != "") Printer.printResults(ctree, outputName)
              else Printer.printResults(ctree)
            case CFP =>
              val globalFormats = formats ++ inputs.mapValues((x: (FixedPointFormat, Interval)) => x._1)
              val ftree = FixedPointCodeGenerator.translateToFP(ctree, globalFormats)
              if (ftree != null)
                println(Printer.getC(ftree))
            case ScalaFP =>
              val globalFormats = formats ++ inputs.mapValues((x: (FixedPointFormat, Interval)) => x._1)
              val ftree = FixedPointCodeGenerator.translateToFP(ctree, globalFormats)
              val paramNames: Iterable[String] = inputs.keys
              if (ftree != null)
                println(Printer.getScala(ftree, paramNames))
          }

        case Expr =>
          var expressions: List[CExpr] = List.empty
          if (multipleExpressions) {
            for (line <- Source.fromFile(programFile).getLines()) {
              if (! (line.length == 0 || line.startsWith("//"))) {  //ignore empty lines
                val tmp = ExprParser.parse(line)
                if (tmp != null) expressions :+= tmp
                else println("WARNING: syntax error")
              }
            }
          } else {
            val ctree = ExprParser.parseFile(programFile)
            if (ctree == null) { println("Parse error."); return }
            expressions :+= ctree
          }
          var counter = 0
          for (expr <- expressions) {
            val output = FixedPointAnalyzer.analyzeExpr(expr, inputs, bits, rounding)
            print("Expr "+counter+" max error: " + xfp.utils.printFloat(output.maxAbsError.toDouble))
            // Matlab needs this printed to a file
            if (counter == 0 && callingFromMatlab) {
              val fw = new FileWriter("error_result")
              fw.write(xfp.utils.printFloat(output.maxAbsError.toDouble))
              fw.close()
            }
            counter += 1
            //println("\nInterval subdivision...")
            //val maxError = IntervalDivision.computeError(inputs, expr, inputs.keys.toList, 5)
            //println("max error: " + maxError)

            outputMode match {
              case Analysis =>
                if (outputName != "") Printer.printResults(expr, outputName)
                else Printer.printResults(expr)
                println("")
              case CFP =>
                val stmts = new Queue[CStmt]()
                val map = new collection.mutable.HashMap[String, FixedPointFormat]()
                FixedPointCodeGenerator.count = 0
                FixedPointCodeGenerator.treeToCode(expr, stmts, map)
                val ftree = FixedPointCodeGenerator.translateToFP(CBasicBlock(stmts.toList), map.toMap)
                println(Printer.getC(ftree))
              case ScalaFP =>
                val stmts = new Queue[CStmt]()
                val map = new collection.mutable.HashMap[String, FixedPointFormat]()
                FixedPointCodeGenerator.count = 0
                FixedPointCodeGenerator.treeToCode(expr, stmts, map)
                val ftree = FixedPointCodeGenerator.translateToFP(CBasicBlock(stmts.toList), map.toMap)
                val paramNames: Iterable[String] = inputs.keys

                if (simulationCount > 0) {
                  print(",  Simulated ("+simulationCount+"): ")
                  val error = xfp.analysis.Simulation.simulate(expr,
                    ftree.asInstanceOf[FBasicBlock].stmts, inputs, simulationCount)
                  println("maxError = " + error)
                }
                else {
                  println(Printer.getScala(ftree, paramNames))
                }
            }
          }
          println()
      }
    } catch {
      case e: java.io.FileNotFoundException =>
        println("!!!  Input file not found: " + e.getMessage); return
      case e: xfp.fixedpoint.FixedPointOverflowException =>
        println("!!! Potential fixed-point overflow detected: " + e.getMessage)
      case e: xfp.fixedpoint.DivisionByZeroException =>
        println("!!! Potential division by zero detected: " + e.getMessage)
    }


  }

  def printHelpOptions = {
    println("xfp - the fixed-point program analyzer (C) Copyright 2012 LARA. EPFL\n"+
      "Usage ./xfp.sh [options] [-c | -s | -a] [-code | -expr] programFileName -inputs inputsFileName\n"+
      "\n"+
      "Options:\n"+
      "\t-b no.bits\tlengths of bitvectors\n"+
      "\t-o outputName\tname of the output variable\n"+
      "\t-r\t\tassume rounding is available\n"+
      "\t-h\t\tprint this help\n"+
      "\t-c\t generate C-like fixed-point code\n"+
      "\t-s\t generate Scala-like fixed-point code\n"+
      "\t-a\t print analysis results\n"+
      "\n"+
      "\n"+
      "The input file must contain the input info in the following format:\n"+
      "In1 In1 1 16 8 -5.3 23            (specifying format and intervals)\n"+
      "Lines starting with 'Block' or 'End' are ignored.\n"+
      "Formats of intermediate variables can be specified in the following format:\n"+
      "Add Add 1 16 9                    (i.e. without the interval bounds)\n"+
      "If at least one such line is present, then it is assumed that formats are\n"+
      "specified for all intermediate variables, including the output variable.\n"+
      "In this case overflow is not checked and formats are forced as specified.\n"+
      "If no format except for input variables is specified, then formats are\n"+
      "automatically determined from the ranges of each computation.\n"+
      "Due to overapproximations, these need not be the most optimal ones.\n"
      )
  }



}
