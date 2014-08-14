package ec.app.fixedpoint;

import utils.parser.ExpressionParser;

import ec.*;
import ec.util.*;
import ec.gp.*;


/**
 * ExpressionBuilder is a GPNodeBuilder that builds a bunch of trees,
 * all equivalent wrt to a real number semantics to an initially given tree.
 * Ignores most of the usual GP settings.
 */
public class ExpressionBuilder extends GPNodeBuilder {
  public static final String P_PROBLEM_NAME = "problem-name";
  public static final String P_FILE_NAME = "file-name";
  public static final String P_NODE_SELECTOR = "node-selector";

  public static FPNode initialTree;
  public static String expressionFileName;

  public void setup(final EvolutionState state, final Parameter base) {
    super.setup(state, base);

    String problemName = state.parameters.getString(base.push(P_PROBLEM_NAME), null);
    String fileName = state.parameters.getString(base.push(P_FILE_NAME), null);
    expressionFileName = fileName;

    // Add labels for crossover. Each label is the String representation of
    // the subtree and marks equivalent trees for later.
    initialTree = ExpressionParser.parseFile(fileName);
    addLabels(initialTree);
  }

  public Parameter defaultBase() { return GPDefaults.base(); }

  public GPNode newRootedTree(final EvolutionState state, final GPType type,
      final int thread, final GPNodeParent parent, final GPFunctionSet set,
      final int argposition, final int requestedSize) {

    return initialTree;
  }

  private void addLabels(FPNode n) {
    n.label = Utils.printTree(n);
    if (n instanceof Add || n instanceof Mult) {
      addLabels((FPNode)n.children[0]);
      addLabels((FPNode)n.children[1]);
    }
    else if (n instanceof Neg || n instanceof Inv) {
      addLabels((FPNode)n.children[0]);
    }
  }

}
