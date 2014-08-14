package ec.app.fixedpoint;

import ec.*;
import ec.gp.*;
import ec.util.*;
import xfp.trees.*;

public abstract class FPNode extends GPNode {
  // Keeps track of where this node comes from,
  // i.e. the original expression
  // if this label is null, we don't know or lost the information
  public String label;

}
