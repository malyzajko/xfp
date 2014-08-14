package ec.app.fixedpoint;

import ec.util.*;
import ec.*;
import ec.gp.*;
import ec.gp.koza.*;
import ec.simple.*;

import java.util.*;

import xfp.fixedpoint.*;
import xfp.analysis.*;
import xfp.trees.*;

/**
 * This is our fitness function.
 */
public class FixedpointOptimization extends GPProblem implements SimpleProblemForm {

  public static final String P_DATA = "data";
  public static final String P_FILE_NAME = "file-name";
  public static final String P_BITLENGTH = "bitlength";
  public static final String P_DEBUG_INFO = "debug-info";
  public static boolean debugInfo;
  public ExprData input;
  public int bitlength;
  final public boolean ROUNDING = false;

  // For some weird reason the sorting algorithm does not seem to work
  // for small floating-point values. So we will scale all fitness values
  // by the initial expressions' fitness (ie. error).
  public static double fitness_scale;

  /*
   * Our own analysis tools.
   */
  // Smallest max. error seen so far (error of the expression in bestTree).
  public static double bestError;
  // Expression with the smallest max error seen so far.
  public static String bestTree = "";

  /*
   * Debugging tools.
   */
  // Collects the unique the trees seen so far
  public static HashSet<CExpr> uniqueTrees;
  // Total number of expressions considered.
  public static long totalNumExprs = 0l;
  // Number of new expressions for each generation.
  public static int[] exprFoundInGeneration = new int[30];
  // Number of new best expressions for each generation.
  public static int[] bestFoundInGeneration = new int[30];

  Map<String, FixedPointFormat> inputFormats = new HashMap<String, FixedPointFormat>();
  Map<String, Interval> inputRanges = new HashMap<String, Interval>();

  public Object clone() {
    FixedpointOptimization newobj = (FixedpointOptimization) (super.clone());
    newobj.input = (ExprData)(input.clone());
    return newobj;
  }

  // Initialize
  public void setup(final EvolutionState state, final Parameter base) {
    super.setup(state,base); // very important, remember this
    input = (ExprData) state.parameters.getInstanceForParameterEq(base.push(P_DATA),
        null, ExprData.class);
    input.setup(state, base.push(P_DATA));

    String fileName = state.parameters.getString(base.push(P_FILE_NAME), null);

    scala.Tuple2<Map<String, FixedPointFormat>, Map<String, Interval>> tuple =
      InputVariablesParser.parseFileJava(fileName);

    inputFormats = tuple._1;
    inputRanges = tuple._2;

    bitlength = state.parameters.getInt(base.push(P_BITLENGTH), null);
    debugInfo = state.parameters.getBoolean(base.push(P_DEBUG_INFO), null, false);
    uniqueTrees = new HashSet<CExpr>();

    CExpr cexpr = ExprParser.parseFile(ExpressionBuilder.expressionFileName);
    FixedForm ff = FixedPointAnalyzer.analyzeExprJava(cexpr, inputFormats, inputRanges,
        bitlength, ROUNDING);
    fitness_scale = ff.maxAbsError().toDouble();

    //fitness_scale = xfp.analysis.RangeSimulation.simulateExprJava(cexpr, inputFormats,
    //      inputRanges, bitlength, 500);

    System.out.println("Initial error = " + fitness_scale);
    // We can just as well use the original error as a baseline.
    bestError = fitness_scale;
  }

  // Evaluates individual and sets its fitness.
  public void evaluate(final EvolutionState state, final Individual ind,
      final int subpopulation, final int threadnum) {
    if (!ind.evaluated) { // don't bother reevaluating
      // Get the expression tree
      ((GPIndividual)ind).trees[0].child.eval(state, threadnum, input, stack,
        ((GPIndividual)ind), this);

      FixedForm ff = FixedPointAnalyzer.analyzeExprJava(input.x, inputFormats,
          inputRanges, bitlength, ROUNDING);
      double abs_error = ff.maxAbsError().toDouble();
      //double abs_error = xfp.analysis.RangeSimulation.simulateExprJava(input.x, inputFormats,
      //    inputRanges, bitlength, 500);

      double scaled_error = abs_error / fitness_scale;

      KozaFitness f = ((KozaFitness)ind.fitness);
      // In GP, 0 is the ideal fitness, infinity worse than the worst possible fitness
      f.setStandardizedFitness(state, (float) scaled_error);

      ind.evaluated = true;
      //For a full dump of info:
      //System.out.println(abs_error);
      //System.out.println("   --   scaled error: " + scaled_error);

      if (abs_error < bestError) {
        bestError = abs_error;
        bestTree = input.x.toString();

        // ==> Comment out this for clean runtimes...
        //System.out.println("found best");
        //bestFoundInGeneration[state.generation]++;
      }
      /*if (!uniqueTrees.contains(input.x)) {
        uniqueTrees.add(input.x);
        exprFoundInGeneration[state.generation]++;
      }
      totalNumExprs++;
      System.out.println(abs_error);
      */
    }
    // For a full dump of info:
    /*else {
      double scaled_error = ((KozaFitness)ind.fitness).standardizedFitness();
      System.out.println(scaled_error * fitness_scale );//+
          //"   ==   scaled error: " + scaled_error);
    }*/
  }
}
