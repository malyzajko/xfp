package ec.app.fixedpoint;

import ec.*;
import ec.util.*;
import ec.gp.*;
import ec.gp.breed.GPBreedDefaults;

/**
 *  Takes two trees, selects a random node from one, if it can find
 *  an equivalent subtree in the other, it crosses over and returns
 *  both trees
 */
public class CrossoverPipeline extends GPBreedingPipeline {

  public static final int NUM_SOURCES = 2;
  public GPNodeSelector nodeselect;
  public Parameter defaultBase() { return GPBreedDefaults.base(); }
  public static int maxRetries;

  // Measures how many times in a generation we succeed in doing crossover.
 // public static int[] successInGeneration = new int[10];
  //public static int[] failInGeneration = new int[10];

  // temporary placeholder
  GPIndividual parents[];
  public int numSources() { return NUM_SOURCES; }

  public Object clone() {
    CrossoverPipeline c = (CrossoverPipeline)(super.clone());
    c.nodeselect = (GPNodeSelector)(nodeselect.clone());
    c.parents = (GPIndividual[])parents.clone();
    return c;
  }

  public void setup(final EvolutionState state, final Parameter base) {
    super.setup(state, base);
    Parameter p = base.push(P_NODESELECTOR).push(""+0);
    Parameter def = defaultBase();
    parents = new GPIndividual[2];

    nodeselect = (GPNodeSelector)
      (state.parameters.getInstanceForParameter(
                p,def.push(P_NODESELECTOR).push(""+0),GPNodeSelector.class));
    nodeselect.setup(state, p);
    maxRetries = 3;
  }

  /** Produces n individuals from the given subpopulation and puts them into
   * inds[start...start+n-1], where n = Min(Max(q,min),max), where q is the
   * "typical" number of individuals the BreedingSource produces in one shot,
   * and returns n.  max must be >= min, and min must be >= 1. For example,
   * crossover might typically produce two individuals, tournament selection
   * might typically produce a single individual, etc.
   */
  public int produce(final int min, final int max, final int start, final int subpopulation,
      final Individual[] inds, final EvolutionState state, final int thread) {
    if (!state.random[thread].nextBoolean(likelihood)) {
      return reproduce(2, start, subpopulation, inds, state, thread, true);
    }

    int n = Math.min(Math.max(2, min), max);

    // In our case, they always produce 1 individual each
    sources[0].produce(1, 1, 0, subpopulation, parents, state, thread);
    sources[1].produce(1, 1, 1, subpopulation, parents, state, thread);

    parents[0] = (GPIndividual)(parents[0].clone());
    parents[1] = (GPIndividual)(parents[1].clone());

    FPNode t1 = (FPNode)parents[0].trees[0].child;
    FPNode t2 = (FPNode)parents[1].trees[0].child;

    // pick a random node from tree 1
    FPNode p1 = (FPNode)nodeselect.pickNode(state, subpopulation, thread, parents[0],
        parents[0].trees[0]);
    FPNode p2 = findEquivalentNode(t2, p1.label);
    int tryCount = 0;
    while ((p2 == null || p1.parent instanceof ec.gp.GPTree) && tryCount < maxRetries) {
      p1 = (FPNode)nodeselect.pickNode(state, subpopulation, thread, parents[0],
        parents[0].trees[0]);
      p2 = findEquivalentNode(t2, p1.label);
      tryCount++;
    }

    // we found an equivalent one, let's do the crossover
    if ( p2 != null && !(p1.parent instanceof ec.gp.GPTree)) {
      // swapping code from ec.gp.breed.InterbalCrossoverPipeline
      GPNodeParent oldparent = p1.parent;
      byte oldargposition = p1.argposition;
      p1.parent = p2.parent;
      p1.argposition = p2.argposition;
      p2.parent = oldparent;
      p2.argposition = oldargposition;
      ((FPNode)(p1.parent)).children[p1.argposition] = p1;
      ((FPNode)(p2.parent)).children[p2.argposition] = p2;
      parents[0].evaluated = false;
      parents[1].evaluated = false;
      //successInGeneration[state.generation / 5]++;
    }
    /*else {
      failInGeneration[state.generation / 5]++;
    }*/

    // We always produce two crossed-over individuals, but we may only have space for one...
    inds[start] = parents[0];
    if (n > 1) {
      inds[start + 1] = parents[1];
    }

    return n;
  }

  /**
   *  Returns a reference to a subtree in tree that is equivalent
   *  (FPNode.labels are the same) to node.
   *  @return reference to subtree from tree
   */
  private FPNode findEquivalentNode(FPNode tree, String id) {
    if (tree.label != null && tree.label.equals(id)) {
      return tree;
    }
    else if (tree instanceof Add || tree instanceof Mult) {
      FPNode tmp = findEquivalentNode((FPNode)tree.children[0], id);
      if (tmp != null) return tmp;
      else return findEquivalentNode((FPNode)tree.children[1], id);
    }
    else if (tree instanceof Neg || tree instanceof Inv) {
      return findEquivalentNode((FPNode)tree.children[0], id);
    }
    else { // Variable or Constant whose label does not match
      return null;
    }
  }


}
