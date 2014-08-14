package ec.app.fixedpoint;

import ec.*;
import ec.gp.*;
import ec.util.*;
import xfp.trees.*;

public class Constant extends FPNode {

  public Constant() {}
  public Constant(double v) {
    children = new FPNode[0]; // not memory optimal...
    value = v;
  }
  public Constant(double v, String lbl) {
    children = new FPNode[0]; // not memory optimal...
    value = v;
    label = lbl;
  }



  public double value = 0.0;

  // Structural equals
  @Override public boolean equals(Object other) {
    if (other instanceof Constant) {
      Constant that = (Constant) other;
      return this.value == that.value;
    }
    else return false;
  }

  @Override public int hashCode() {
    return new Double(value).hashCode();
  }

  public String toString() { return "" + value; }

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
    rd.x = new CDoubleConst(value);
  }
}
