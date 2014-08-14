package ec.app.fixedpoint;

import ec.*;
import ec.gp.*;
import ec.util.*;
import xfp.trees.*;

public class Inv extends FPNode {

  public Inv() {}
  public Inv(FPNode expr) {
    children = new FPNode[1];
    expr.parent = this;
    children[0] = expr;
  }
  public Inv(FPNode expr, String lbl) {
    children = new FPNode[1];
    expr.parent = this;
    children[0] = expr;
    label = lbl;
  }


  // Structural equals
  @Override public boolean equals(Object other) {
    if (other instanceof Inv) {
      Inv that = (Inv) other;
      return this.children[0].equals(that.children[0]);
    }
    else return false;
  }

  @Override public int hashCode() {
    int hash = 4;
    hash = hash * 31 + children[0].hashCode();
    return hash;
  }

  public String toString() { return "1/"; }

  // Sanity check
  public void checkConstraints(final EvolutionState state, final int tree,
      final GPIndividual typicalIndividual, final Parameter individualBase) {
    super.checkConstraints(state, tree, typicalIndividual, individualBase);
    if (children.length != 1)
      state.output.error("Incorrect number of children for node " +
                         toStringForError() + " at " + individualBase);
  }

  // Get value of this node.
  public void eval(final EvolutionState state, final int thread, final GPData input,
      final ADFStack stack, final GPIndividual individual, final Problem problem) {
    ExprData rd = ((ExprData)(input));
    children[0].eval(state, thread, input, stack, individual, problem);
    rd.x = new CInv(rd.x);
  }
}
