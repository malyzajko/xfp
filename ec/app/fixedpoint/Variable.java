package ec.app.fixedpoint;

import ec.*;
import ec.gp.*;
import ec.util.*;

import xfp.trees.*;

public class Variable extends FPNode {

  private String variableName = "default";

  public Variable() {}
  public Variable(String n) {
    children = new FPNode[0]; // not memory optimal...
    variableName = n;
  }

  public Variable(String n, String lbl) {
    children = new FPNode[0]; // not memory optimal...
    variableName = n;
    label = lbl;
  }


  // Structural equals
  @Override public boolean equals(Object other) {
    if (other instanceof Variable) {
      Variable that = (Variable) other;
      return this.variableName.equals(that.variableName);
    }
    else return false;
  }

  @Override public int hashCode() {
    return variableName.hashCode();
  }

  public String toString() { return variableName; }

  public void checkConstraints(final EvolutionState state, final int tree,
      final GPIndividual typicalIndividual, final Parameter individualBase) {
    super.checkConstraints(state,tree,typicalIndividual,individualBase);
    if (children.length != 0)
      state.output.error("Incorrect number of children for node " +
          toStringForError() + " at " + individualBase);
  }

  public void eval(final EvolutionState state, final int thread, final GPData input,
      final ADFStack stack, final GPIndividual individual, final Problem problem) {
    ExprData rd = ((ExprData)(input));
    rd.x = new CVar(variableName);
  }
}
