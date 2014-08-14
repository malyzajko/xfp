package ec.app.fixedpoint;

import ec.*;
import ec.util.*;
import ec.gp.*;
import ec.gp.breed.GPBreedDefaults;
import java.util.Arrays;
import java.util.Vector;
import java.util.HashSet;
import java.util.Iterator;

/**
 * Selects one node in the tree and applies one rewriting rule on it.
 */
public class MutationPipeline extends GPBreedingPipeline {

  public static final int NUM_SOURCES = 1;
  public GPNodeSelector nodeselect;
  public Parameter defaultBase() { return GPBreedDefaults.base(); }
  public int numSources() { return NUM_SOURCES; }

  // For debugging purposes only, counts how many times we use each rule
  private static long[] mutationCounts;
  private static HashSet<GPNode> generatedTrees;
  private static long numExpressions;  // total number of expressions seen

  public Object clone() {
    MutationPipeline c = (MutationPipeline)(super.clone());
    c.nodeselect = (GPNodeSelector)(nodeselect.clone());
    return c;
  }

  public void setup(final EvolutionState state, final Parameter base) {
    super.setup(state, base);
    Parameter p = base.push(P_NODESELECTOR).push(""+0);
    Parameter def = defaultBase();

    nodeselect = (GPNodeSelector)
      (state.parameters.getInstanceForParameter(
                p,def.push(P_NODESELECTOR).push(""+0),GPNodeSelector.class));
    nodeselect.setup(state, p);
    //mutationCounts = new long[23];
    //generatedTrees = new HashSet<GPNode>();
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
    int n = sources[0].produce(min, max, start, subpopulation, inds, state, thread);

    GPInitializer initializer = ((GPInitializer)state.initializer);

    for (int q = start; q < n + start; q++) {
      GPIndividual i = (GPIndividual)inds[q];
      nodeselect.reset();
      GPNode p1 = nodeselect.pickNode(state, subpopulation, thread, i, i.trees[0]);

      GPNode p2 = generateMutant(p1, state, thread);

      p2.resetNode(state, thread);

      GPIndividual j = (GPIndividual)(i.lightClone());
      j.trees = new GPTree[i.trees.length];
      j.trees[0] = (GPTree)(i.trees[0].lightClone());
      j.trees[0].owner = j;
      j.trees[0].child = i.trees[0].child.cloneReplacing(p2,p1);
      j.trees[0].child.parent = j.trees[0];
      j.trees[0].child.argposition = 0;
      j.evaluated = false;

      inds[q] = j;
    }
    return n;
  }

  /*
   * We may be missing rules like
   * 1/a + 1/b <-> (b + a)/ab
   * (a + b)/c -> a/c + b/c
   */

  public static GPNode generateMutant(GPNode n, final EvolutionState state, final int thread) {
    // First collect all the applicable rules, then randomly pick one of them
    Vector <Integer> rules = new Vector<Integer>();

    if (n instanceof Add) {
      // (a + b) + c -> a + (b + c)
      if (n.children[0] instanceof Add) { rules.add(new Integer(1)); rules.add(new Integer(1)); }
      // a + (b + c) -> (a + b) + c
      if (n.children[1] instanceof Add) { rules.add(new Integer(2)); rules.add(new Integer(2)); }
      if (n.children[0] instanceof Mult && n.children[1] instanceof Mult) {
        // (a * b) + (a * c) -> a * (b + c)
        if(n.children[0].children[0].equals(n.children[1].children[0])) {
          rules.add(new Integer(3)); rules.add(new Integer(3));
        }
        // (a * c) + (b * c) -> (a + b) * c
        if (n.children[0].children[1].equals(n.children[1].children[1])) {
          rules.add(new Integer(4)); rules.add(new Integer(4));
        }
        // (a * b) + (c * a)
        if (n.children[0].children[0].equals(n.children[1].children[1])) {
          rules.add(new Integer(5)); rules.add(new Integer(5));
        }
        // (b * a) + (a * c)
        if (n.children[0].children[1].equals(n.children[1].children[0])) {
          rules.add(new Integer(6)); rules.add(new Integer(6));
        }
      }
      // (-a) + (-b) -> -(a + b)
      if (n.children[0] instanceof Neg && n.children[1] instanceof Neg) {
        rules.add(new Integer(7)); rules.add(new Integer(7));
      }
      // can be always applied: a + b = b + a
      rules.add(new Integer(8));
    }

    else if (n instanceof Mult) {
      // (a * b) * c -> a * (b * c)
      if (n.children[0] instanceof Mult) { rules.add(new Integer(9)); rules.add(new Integer(9)); }
      // a * (b * c) -> (a * b) * c
      if (n.children[1] instanceof Mult) { rules.add(new Integer(10)); rules.add(new Integer(10)); }
      // a * (b + c) -> (a * b) + (a * c)
      if (n.children[1] instanceof Add) { rules.add(new Integer(11)); rules.add(new Integer(11)); }
      // (a + b) * c -> (a * c) + (b * c)
      if (n.children[0] instanceof Add) { rules.add(new Integer(12)); rules.add(new Integer(12)); }
      // (-a) * b -> - (a * b)
      if (n.children[0] instanceof Neg) { rules.add(new Integer(13)); rules.add(new Integer(13)); }
      // a * (-b) -> - (a * b)
      if (n.children[1] instanceof Neg) { rules.add(new Integer(14)); rules.add(new Integer(14)); }
      // 1/a * 1/b -> 1/(ab)
      if (n.children[0] instanceof Inv && n.children[1] instanceof Inv) {
        rules.add(new Integer(15)); rules.add(new Integer(15));
      }
      // can always be applied: a * b = b * a
      rules.add(new Integer(16));
    }

    else if (n instanceof Neg) {
      // -(a * b) -> (-a) * b
      if (n.children[0] instanceof Mult) { rules.add(new Integer(17)); rules.add(new Integer(18)); }
      // - (a + b) -> (-a) + (-b)
      if (n.children[0] instanceof Add) { rules.add(new Integer(19)); }
      // - (1/a) -> 1/ (-a)
      if (n.children[0] instanceof Inv) { rules.add(new Integer(20)); }
    }

    else if (n instanceof Inv) {
      // 1/(-a) -> -(1/a)
      if (n.children[0] instanceof Neg) { rules.add(new Integer(21)); }
      // 1/(ab) -> 1/a * 1/b
      if (n.children[0] instanceof Mult) { rules.add(new Integer(22)); }
    }
    else { // If we get here, something's broken
      state.output.error("Wrong class of node picked for mutation " + n.getClass());
      return null;
    }

    if (rules.size() == 0) return n;  // no applicable rule
    else {
      float rnd = state.random[thread].nextFloat();
      int index = (int) Math.floor(rnd * rules.size());
      return applyRule(n, rules.get(index).intValue(), state);
    }
  }

  private static GPNode applyRule(GPNode gpNode, int index, EvolutionState state) {
    FPNode n = (FPNode)gpNode;
    switch (index) {
      case 1: // (a + b) + c -> a + (b + c)
        {
          FPNode bplusc = new Add((FPNode)n.children[0].children[1], (FPNode)n.children[1]);
          bplusc.label = Utils.printTree(bplusc);
          return new Add((FPNode)n.children[0].children[0], bplusc, n.label);
        }
      case 2: // a + (b + c) -> (a + b) + c
        {
          FPNode aplusb = new Add((FPNode)n.children[0], (FPNode)n.children[1].children[0]);
          aplusb.label = Utils.printTree(aplusb);
          return new Add(aplusb, (FPNode)n.children[1].children[1], n.label);
        }
      case 3: // (a * b) + (a * c) -> a * (b + c)
        {
          FPNode bplusc = new Add((FPNode)n.children[0].children[1], (FPNode)n.children[1].children[1]);
          bplusc.label = Utils.printTree(bplusc);
          return new Mult((FPNode)n.children[0].children[0], bplusc, n.label);
        }
      case 4: // (a * c) + (b * c) -> (a + b) * c
        {
          FPNode aplusb = new Add((FPNode)n.children[0].children[0], (FPNode)n.children[1].children[0]);
          aplusb.label = Utils.printTree(aplusb);
          return new Mult(aplusb, (FPNode)n.children[0].children[1], n.label);
        }
      case 5: // (a * b) + (c * a) -> a * (b + c)
        {
          FPNode bplusc = new Add((FPNode)n.children[0].children[1], (FPNode)n.children[1].children[0]);
          bplusc.label = Utils.printTree(bplusc);
          return new Mult((FPNode)n.children[0].children[0], bplusc, n.label);
        }
      case 6: // (b * a) + (a * c) -> (b + c) * a
        {
          FPNode bplusc = new Add((FPNode)n.children[0].children[0], (FPNode)n.children[1].children[1]);
          bplusc.label = Utils.printTree(bplusc);
          return new Mult(bplusc, (FPNode)n.children[0].children[1], n.label);
        }
      case 7:  // (-a) + (-b) -> -(a + b)
        {
          FPNode aplusb = new Add((FPNode)n.children[0].children[0], (FPNode)n.children[1].children[0]);
          aplusb.label = Utils.printTree(aplusb);
          return new Neg(aplusb, n.label);
        }
      case 8: // can be always applied: a + b = b + a
        return new Add((FPNode)n.children[1], (FPNode)n.children[0], n.label);
      case 9: // (a * b) * c -> a * (b * c)
        {
          FPNode btimesc = new Mult((FPNode)n.children[0].children[1], (FPNode)n.children[1]);
          btimesc.label = Utils.printTree(btimesc);
          return new Mult((FPNode)n.children[0].children[0], btimesc, n.label);
        }
      case 10: // a * (b * c) -> (a * b) * c
        {
          FPNode atimesb = new Mult((FPNode)n.children[0], (FPNode)n.children[1].children[0]);
          atimesb.label = Utils.printTree(atimesb);
          return new Mult(atimesb, (FPNode)n.children[1].children[1], n.label);
        }
      case 11: // a * (b + c) -> (a * b) + (a * c)
        {
          FPNode atimesb = new Mult((FPNode)n.children[0], (FPNode)n.children[1].children[0]);
          FPNode atimesc = new Mult((FPNode)n.children[0], (FPNode)n.children[1].children[1]);
          atimesb.label = Utils.printTree(atimesb);
          atimesc.label = Utils.printTree(atimesc);
          return new Add(atimesb, atimesc, n.label);
        }
      case 12: // (a + b) * c -> (a * c) + (b * c)
        {
          FPNode atimesc = new Mult((FPNode)n.children[0].children[0], (FPNode)n.children[1]);
          FPNode btimesc = new Mult((FPNode)n.children[0].children[1], (FPNode)n.children[1]);
          atimesc.label = Utils.printTree(atimesc);
          btimesc.label = Utils.printTree(btimesc);
          return new Add(atimesc, btimesc, n.label);
        }
      case 13: // (-a) * b -> - (a * b)
        {
          FPNode atimesb = new Mult((FPNode)n.children[0].children[0], (FPNode)n.children[1]);
          atimesb.label = Utils.printTree(atimesb);
          return new Neg(atimesb, n.label);
        }
      case 14: // a * (-b) -> - (a * b)
        {
          FPNode atimesb = new Mult((FPNode)n.children[0], (FPNode)n.children[1].children[0]);
          atimesb.label = Utils.printTree(atimesb);
          return new Neg(atimesb, n.label);
        }
      case 15: // 1/a * 1/b -> 1/(ab)
        {
          FPNode atimesb = new Mult((FPNode)n.children[0].children[0], (FPNode)n.children[1].children[0]);
          atimesb.label = Utils.printTree(atimesb);
          return new Inv(atimesb, n.label);
        }
      case 16: // can always be applied: a * b = b * a
        return new Mult((FPNode)n.children[1], (FPNode)n.children[0], n.label);
      case 17: // -(a * b) -> (-a) * b
        {
          FPNode minusa = new Neg((FPNode)n.children[0].children[0]);
          minusa.label = Utils.printTree(minusa);
          return new Mult(minusa, (FPNode)n.children[0].children[1], n.label);
        }
      case 18: // -(a * b) -> a * (-b)
        {
          FPNode minusb = new Neg((FPNode)n.children[0].children[1]);
          minusb.label = Utils.printTree(minusb);
          return new Mult((FPNode)n.children[0].children[0], minusb, n.label);
        }
      case 19: // - (a + b) -> (-a) + (-b)
        {
          FPNode minusa = new Neg((FPNode)n.children[0].children[0]);
          FPNode minusb = new Neg((FPNode)n.children[0].children[1]);
          minusa.label = Utils.printTree(minusa);
          minusb.label = Utils.printTree(minusb);
          return new Add(minusa, minusb, n.label);
        }
      case 20: // - (1/a) -> 1/ (-a)
        {
          FPNode minusa = new Neg((FPNode)n.children[0].children[0]);
          minusa.label = Utils.printTree(minusa);
          return new Inv(minusa, n.label);
        }
      case 21: // 1/(-a) -> -(1/a)
        {
          FPNode inverse = new Inv((FPNode)n.children[0].children[0]);
          inverse.label = Utils.printTree(inverse);
          return new Neg(inverse, n.label);
        }
      case 22: // 1/(ab) -> 1/a * 1/b
        {
          FPNode invA = new Inv((FPNode)n.children[0].children[0]);
          FPNode invB = new Inv((FPNode)n.children[0].children[1]);
          invA.label = Utils.printTree(invA);
          invB.label = Utils.printTree(invB);
          return new Mult(invA, invB, n.label);
        }
      default:
        state.output.error("Wrong class of node picked for mutation " + n.getClass());
    }
    return null;
  }

}
