package ec.app.fixedpoint;

import ec.gp.*;

public class Utils {

  static String printTree(GPNode node) {
    if (node instanceof Add)
      return "(" + printTree(node.children[0]) + " + " + printTree(node.children[1]) + ")";
    else if (node instanceof Mult)
      return "(" + printTree(node.children[0]) + " * " + printTree(node.children[1]) + ")";
    else if (node instanceof Neg)
      return "(-" + printTree(node.children[0]) + ")";
    else if (node instanceof Inv)
      return "(1/" + printTree(node.children[0]) + ")";
    else if (node instanceof Variable)
      return node.toString();
    else if (node instanceof Constant)
      return node.toString();
    else return "";
  }



}
