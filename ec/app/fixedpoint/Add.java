package ec.app.fixedpoint;

import ec.*;
import ec.gp.*;
import ec.util.*;
import xfp.trees.*;

public class Add extends FPNode {

  public Add() {}
  public Add(FPNode lhs, FPNode rhs) {
    children = new FPNode[2];
    lhs.parent = this;
    rhs.parent = this;
    children[0] = lhs;
    children[1] = rhs;
  }

  public Add(FPNode lhs, FPNode rhs, String lbl) {
    children = new FPNode[2];
    lhs.parent = this;
    rhs.parent = this;
    children[0] = lhs;
    children[1] = rhs;
    label = lbl;
  }


  // Structural equals
  @Override public boolean equals(Object other) {
    if (other instanceof Add) {
      Add that = (Add) other;
      return this.children[0].equals(that.children[0]) &&
        this.children[1].equals(that.children[1]);
    }
    else return false;
  }

  @Override public int hashCode() {
    int hash = 1;
    hash = hash * 31 + children[0].hashCode() + children[1].hashCode();
    return hash;
  }

  public String toString() { return "+"; }

  // Sanity check
  public void checkConstraints(final EvolutionState state, final int tree,
      final GPIndividual typicalIndividual, final Parameter individualBase) {
    super.checkConstraints(state, tree, typicalIndividual, individualBase);
    if (children.length != 2)
      state.output.error("Incorrect number of children for node " +
                         toStringForError() + " at " + individualBase);
  }

  // Get value of this node.
  public void eval(final EvolutionState state, final int thread, final GPData input,
      final ADFStack stack, final GPIndividual individual, final Problem problem) {
    ExprData rd = ((ExprData)(input));

    children[0].eval(state, thread, input, stack, individual, problem);
    CExpr result = rd.x;

    children[1].eval(state, thread, input, stack, individual, problem);
    rd.x = new CAdd(result, rd.x);
  }

}
