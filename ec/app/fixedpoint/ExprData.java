package ec.app.fixedpoint;

import ec.util.*;
import ec.*;
import ec.gp.*;

import xfp.trees.*;

public class ExprData extends GPData {
  public CExpr x;  // return value

  // copy content to other AffineData
  public void copyTo(final GPData gpd) {
    ((ExprData) gpd).x = x;
  }
}

