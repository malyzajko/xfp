/*
  Copyright 2006 by Sean Luke
  Licensed under the Academic Free License version 3.0
  See the file "LICENSE" for more information
*/


package ec.simple;
import ec.*;
import ec.steadystate.*;
import java.io.IOException;
import ec.util.*;
import java.io.File;
import ec.app.fixedpoint.FixedpointOptimization;
import ec.app.fixedpoint.CrossoverPipeline;

/* 
 * SimpleStatistics.java
 * 
 * Created: Tue Aug 10 21:10:48 1999
 * By: Sean Luke
 */

/**
 * A basic Statistics class suitable for simple problem applications.
 *
 * SimpleStatistics prints out the best individual, per subpopulation,
 * each generation.  At the end of a run, it also prints out the best
 * individual of the run.  SimpleStatistics outputs this data to a log
 * which may either be a provided file or stdout.  Compressed files will
 * be overridden on restart from checkpoint; uncompressed files will be 
 * appended on restart.
 *
 * <p>SimpleStatistics implements a simple version of steady-state statistics:
 * if it quits before a generation boundary,
 * it will include the best individual discovered, even if the individual was discovered
 * after the last boundary.  This is done by using individualsEvaluatedStatistics(...)
 * to update best-individual-of-generation in addition to doing it in
 * postEvaluationStatistics(...).

 <p><b>Parameters</b><br>
 <table>
 <tr><td valign=top><i>base.</i><tt>gzip</tt><br>
 <font size=-1>boolean</font></td>
 <td valign=top>(whether or not to compress the file (.gz suffix added)</td></tr>
 <tr><td valign=top><i>base.</i><tt>file</tt><br>
 <font size=-1>String (a filename), or nonexistant (signifies stdout)</font></td>
 <td valign=top>(the log for statistics)</td></tr>
 </table>

 *
 * @author Sean Luke
 * @version 1.0 
 */

public class SimpleStatistics extends Statistics implements SteadyStateStatisticsForm //, ec.eval.ProvidesBestSoFar
    {
    public Individual[] getBestSoFar() { return best_of_run; }

    /** log file parameter */
    public static final String P_STATISTICS_FILE = "file";
      
    /** compress? */
    public static final String P_COMPRESS = "gzip";

    /** The Statistics' log */
    public int statisticslog;

    /** The best individual we've found so far */
    public Individual[] best_of_run;
        
    /** Should we compress the file? */
    public boolean compress;

    public long startTime;


    public static final String P_NAME_OF_RUN = "name-of-run";
    public String name_of_run;

    public SimpleStatistics() { best_of_run = null; statisticslog = 0; /* stdout */ }

    public void setup(final EvolutionState state, final Parameter base)
        {
        super.setup(state,base);

        compress = state.parameters.getBoolean(base.push(P_COMPRESS),null,false);
        name_of_run = state.parameters.getString(base.push(P_NAME_OF_RUN), null);

        File statisticsFile = state.parameters.getFile(
            base.push(P_STATISTICS_FILE),null);

        if (statisticsFile!=null)
            try
                {
                statisticslog = state.output.addLog(statisticsFile, !compress, compress);
                }
            catch (IOException i)
                {
                state.output.fatal("An IOException occurred while trying to create the log " + statisticsFile + ":\n" + i);
                }

        // Remember starting time for getting the overall runtime.
        startTime = System.currentTimeMillis();
        }

    public void postInitializationStatistics(final EvolutionState state)
        {
        super.postInitializationStatistics(state);
        
        // set up our best_of_run array -- can't do this in setup, because
        // we don't know if the number of subpopulations has been determined yet
        best_of_run = new Individual[state.population.subpops.length];
        }

    /** Logs the best individual of the generation. */
    public void postEvaluationStatistics(final EvolutionState state)
        {
        // Need the line to see different generations when we do ... | tee dump_file.txt
        super.postEvaluationStatistics(state);
        
        // for now we just print the best fitness per subpopulation.
        /*Individual[] best_i = new Individual[state.population.subpops.length];  // quiets compiler complaints
        for(int x=0;x<state.population.subpops.length;x++)
            {
            best_i[x] = state.population.subpops[x].individuals[0];
            for(int y=1;y<state.population.subpops[x].individuals.length;y++)
                if (state.population.subpops[x].individuals[y].fitness.betterThan(best_i[x].fitness))
                    best_i[x] = state.population.subpops[x].individuals[y];
        
            // now test to see if it's the new best_of_run
            if (best_of_run[x]==null || best_i[x].fitness.betterThan(best_of_run[x].fitness))
                best_of_run[x] = (Individual)(best_i[x].clone());
            }
        
        // print the best-of-generation individual
        state.output.println("\nGeneration: " + state.generation,statisticslog);
        state.output.println("Best Individual:",statisticslog);
        for(int x=0;x<state.population.subpops.length;x++)
            {
            state.output.println("Subpopulation " + x + ":",statisticslog);
            best_i[x].printIndividualForHumans(state,statisticslog);
            state.output.message("Subpop " + x + " best fitness of generation: " + best_i[x].fitness.fitnessToStringForHumans());
            }
        */
        }

    /** Logs the best individual of the run. */
    public void finalStatistics(final EvolutionState state, final int result)
        {
        // Total runtime...
        long totalTime = System.currentTimeMillis() - startTime;

        super.finalStatistics(state,result);

        // for now we just print the best fitness
        // We have our own statistic...
        /*state.output.println("\nBest Individual of Run:",statisticslog);
        for(int x=0;x<state.population.subpops.length;x++ )
            {
            state.output.println("Subpopulation " + x + ":",statisticslog);
            best_of_run[x].printIndividualForHumans(state,statisticslog);
            state.output.message("Subpop " + x + " best fitness of run: " + best_of_run[x].fitness.fitnessToStringForHumans());

            // finally describe the winner if there is a description
            if (state.evaluator.p_problem instanceof SimpleProblemForm)
                ((SimpleProblemForm)(state.evaluator.p_problem.clone())).describe(state, best_of_run[x], x, 0, statisticslog);      
            }
        */
        // Our individual stuff:
        try {
          java.io.FileWriter out = new java.io.FileWriter("results.txt", true);
          out.write("\n\n" + name_of_run + "\n");
          out.write("Runtime: " + totalTime + "ms\n");
          out.write("Best seen tree:\n" + FixedpointOptimization.bestTree);
          out.write("\nError: " + FixedpointOptimization.bestError);
          /*out.write("\n# exprs considered: " + FixedpointOptimization.totalNumExprs +
              ", # unique exprs found: " + FixedpointOptimization.uniqueTrees.size());
          */
          //int[] exprs = FixedpointOptimization.exprFoundInGeneration;
          //out.write("\nNew exprs up to generation\n"+
          //    "  5,  10,  15,  20,  25,  30,  35,  40,  45,  50\n");
          //out.write("\nNew expressions found per generation:");
          //for (int i = 0; i < 30; i++) { out.write(String.format("%3d, ", exprs[i])); }

          //exprs = FixedpointOptimization.bestFoundInGeneration;
          //out.write("\nBest exprs up to generation\n"+
          //    "  5,  10,  15,  20,  25,  30,  35,  40,  45,  50\n");
          //out.write("\nBest expressions found per generation:");
          //for (int i = 0; i < 30; i++) { out.write(String.format("%3d, ", exprs[i])); }

          /*
          int[] succ = CrossoverPipeline.successInGeneration;
          int[] fail = CrossoverPipeline.failInGeneration;
          out.write("\nSuccessfull crossover up to generation\n"+
              " 5,   10,   15,   20,   25,   30,   35,   40,   45,   50\n");
          for (int i = 0; i < 10; i++) {
            double total = (double)(succ[i] + fail[i]);
            if (total != 0) out.write( String.format("%.2f, ", (succ[i] / total)));
            else out.write( " -, ");
          }
          */
          out.close();

          // This is for Matlab:
          out = new java.io.FileWriter("error_result", false);
          out.write("" + FixedpointOptimization.bestError);
          out.close();
        } catch (IOException e) {
          System.out.println("Writing to file failed.");
        }

        }

   }
