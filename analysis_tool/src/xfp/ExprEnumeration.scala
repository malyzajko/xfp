
import xfp.trees._
import collection.mutable.{HashSet, Set}

/**
  * Generates all possible rewritings of an expression given by the input file.
  *
  * The algorithm works as follows:
  * 0) Add the expression to the worklist
  * 1) For all expressions e in the worklist:
  *   - For all nodes n in the expression e:
  *      - apply all possible rewrite rules to node n,
  *        generating new expressions
  *      - if a new expression is not yet in the worklist, add it
  *        to a set 'newExprs', and set 'changed' to true
  *  2) Add all expressions in newExprs to worklist
  *  3) If changed is true, repeat from step 1
  *
  * Takes as input the file with the expression and optionally the size the
  * worklist can maximally grow to before giving up (defualt: 10000).
  * If the algorithm does not finish, 100 randomly selected expressions are
  * printed to the file random100.txt in addition to the generated.txt
  */
object ExprEnumeration {
  val debug = false
  // all expressions seen so far
  var worklist = new HashSet[CExpr]()

  var maxWorklistSize = 10000000

  def main(args: Array[String]) {
    if (args.size == 0) { println("Please specify the input file with the expression"); return }
    val origExpr = ExprParser.parseFile(args(0)).asInstanceOf[CExpr]
    if (origExpr == null) { println("Parse error."); return }
    val original = translate(origExpr)

    if (args.size > 1) maxWorklistSize = args(1).toInt

    worklist += original
    var step = 0
    var changed = true

    while (changed && worklist.size < maxWorklistSize) {
      if (debug) println("\n\nstep " + step + ". worklist: " + worklist.size)
      var newExprs = new HashSet[CExpr]()
      changed = false

      for (expr <- worklist) {
        var allSubtrees = allNodes(expr)
        for (node <- allSubtrees) {
          var newNodes = applyRules(node)
          for (newNode <- newNodes) {
            val tmp = findAndReplace(expr, node, newNode)
            if (!worklist.contains(tmp)) {
              newExprs += tmp
              changed = true
            }
          }
        }
      }
      if (debug) println("new expressions: " + newExprs)
      if (newExprs.size > 0) {
        assert(changed)
        worklist ++= newExprs
      }
      step += 1
    }
    // Now cleanup all the duplicates
    val finished = worklist.map(e => cleanUp(orderLex(e)._1))
    println("Generated expressions: " + finished.size)
    var index = 0
    /*val map = Map("x0" -> "", "x1" -> "")*/

    outputToFile("generated.txt", finished)

    if (worklist.size >= maxWorklistSize) {
      println("WARNING: did not finish!")
      outputToFile("random900.txt", util.Random.shuffle(finished.toList).slice(0, 900))
    }

  }

  def outputToFile(name: String, set: Iterable[CExpr]) = {
    var index = 0
    val out = new java.io.FileWriter(name)
    for(e <- set) {
      //println(e)
      //out.write(e.toString(map))
      out.write("//expr " + index + "\n")
      out.write(e.toString() + "\n")
      index += 1
    }
    out.close
  }

  // bottom- up
  // return the sorted expression, plus a string representation, used for comparison
  def orderLex(e: CExpr): (CExpr, String) = e match {
    case CAdd(a, b) =>
      val (aExp, aStr) = orderLex(a)
      val (bExp, bStr) = orderLex(b)
      if (bStr.compareTo(aStr) < 0) (CAdd(bExp, aExp), bStr + aStr)
      else (CAdd(aExp, bExp), aStr + bStr)

    case CMult(a, b) =>
      val (aExp, aStr) = orderLex(a)
      val (bExp, bStr) = orderLex(b)
      if (bStr.compareTo(aStr) < 0) (CMult(bExp, aExp), bStr + aStr)
      else (CMult(aExp, bExp), aStr + bStr)

    case CNeg(a) =>
      val (aExp, aStr) = orderLex(a)
      (CNeg(aExp), aStr)

    case CInv(a) =>
      val (aExp, aStr) = orderLex(a)
      (CInv(aExp), aStr)

    case CDoubleConst(a) => (e, a.toString)

    case CVar(n) => (e, n)

    case _ => return null;
  }

  def cleanUp(e: CExpr): CExpr = e match {
    case CAdd(a, CNeg(b)) => CSub(cleanUp(a), cleanUp(b))
    case CAdd(a, b) => CAdd(cleanUp(a), cleanUp(b))
    case CMult(a, b) => CMult(cleanUp(a), cleanUp(b))
    case CInv(a) => CInv(cleanUp(a))
    case CNeg(a) => CNeg(cleanUp(a))
    case CDoubleConst(d) => CDoubleConst(d)
    case CVar(n) => CVar(n)
    case _ => return null;
  }

  def translate(e: CExpr): CExpr = e match {
    case CSub(a, b) => CAdd(translate(a), CNeg(translate(b)))
    case CAdd(a, b) => CAdd(translate(a), translate(b))
    case CMult(a, b) => CMult(translate(a), translate(b))
    case CDiv(a, b) => CMult(translate(a), CInv(translate(b)))
    case CInv(a) => CInv(translate(a))
    case CNeg(a) => CNeg(translate(a))
    case CDoubleConst(d) => CDoubleConst(d)
    case CVar(n) => CVar(n)
    case _ => return null;
  }

  def allNodes(tree: CExpr): Set[CExpr] = tree match {
    case CNeg(a) => Set(tree) ++ allNodes(a)
    case CAdd(a, b) => Set(tree) ++ allNodes(a) ++ allNodes(b)
    case CMult(a, b) => Set(tree) ++ allNodes(a) ++ allNodes(b)
    case CInv(a) => Set(tree) ++ allNodes(a)
    case CDoubleConst(a) => Set(tree)
    case CVar(a) => Set(tree)
    case _ => return null;
  }


  def applyRules(expr: CExpr): Set[CExpr] = {
    val tmp: Set[CExpr] = expr match {
      case CNeg(CMult(a, b)) => Set( CMult(CNeg(a), b), CMult(a, CNeg(b)) )
      case CNeg(CAdd(a, b)) => Set( CAdd(CNeg(a), CNeg(b)))
      case CNeg(CInv(a)) => Set(CInv(CNeg(a)))

      case CAdd(CAdd(a, b), CAdd(c, d)) =>
        Set(  CAdd(a, CAdd(b, CAdd(c, d))), CAdd(CAdd(CAdd(a, b), c), d),
              CAdd(CAdd(c, d), CAdd(a, b)) )

      case CAdd(CAdd(a, b), c) => Set( CAdd(a, CAdd(b, c)), CAdd(c, CAdd(a, b)) )
      case CAdd(a, CAdd(b, c)) => Set( CAdd(CAdd(a, b), c), CAdd(CAdd(b, c), a) )
      case CAdd(CMult(a, b), CMult(c, d)) =>
        var list = new HashSet[CExpr]()
        if (a == c) list += CMult(a, CAdd(b, d))
        if (b == d) list += CMult(CAdd(a, c), b)
        if (a == d) list += CMult(a, CAdd(b, c))
        if (b == c) list += CMult(CAdd(a, d), b)
        list += CAdd(CMult(c, d), CMult(a, b))

      case CAdd(CNeg(a), CNeg(b)) => Set( CNeg(CAdd(a, b)), CAdd(CNeg(b), CNeg(a)))
      case CAdd(a, b) => Set(CAdd(b, a))

      case CMult(CMult(a, b), c) => Set(CMult(a, CMult(b, c)), CMult(c, CMult(a, b)))
      case CMult(a, CMult(b, c)) => Set(CMult(CMult(a, b), c), CMult(CMult(b, c), a))

      case CMult(a, CAdd(b, c)) => Set(CAdd(CMult(a, b), CMult(a, c)),  CMult(CAdd(b, c), a))

      case CMult(CAdd(a, b), c) => Set(CAdd(CMult(a, c), CMult(b, c)), CMult(c, CAdd(a, b)))
      case CMult(CNeg(a), b) => Set(CNeg(CMult(a, b)), CMult(b, CNeg(a)))
      case CMult(a, CNeg(b)) => Set(CNeg(CMult(a, b)), CMult(CNeg(b), a))
      case CMult(CInv(a), CInv(b)) => Set(CInv(CMult(a, b)), CMult(CInv(b), CInv(a)))
      case CMult(a, b) => Set(CMult(b, a))

      case CInv(CNeg(a)) => Set(CNeg(CInv(a)))
      case CInv(CMult(a, b)) => Set(CMult(CInv(a), CInv(b)))
      case _ => Set( )
    }
    //for (e <- tmp) if (e.getClass == expr.getClass) markDone(e)
    tmp
  }

  // FIXME: if we have duplicates, we need to replace all!
  def findAndReplace(tree: CExpr, oldNode: CExpr, newNode: CExpr): CExpr =
    if (tree == oldNode) return newNode
    else
      tree match {
        case CAdd(l, r) =>
          if (l == oldNode) CAdd(newNode, r)
          else if (r == oldNode) CAdd(l, newNode)
          else CAdd(findAndReplace(l, oldNode, newNode), findAndReplace(r, oldNode, newNode))

        case CMult(l, r) =>
          if (l == oldNode) CMult(newNode, r)
          else if (r == oldNode) CMult(l, newNode)
          else CMult(findAndReplace(l, oldNode, newNode), findAndReplace(r, oldNode, newNode))

        case CNeg(e) =>
          if (e == oldNode) CNeg(newNode)
          else CNeg(findAndReplace(e, oldNode, newNode))

        case CInv(e) =>
          if (e == oldNode) CInv(newNode)
          else CInv(findAndReplace(e, oldNode, newNode))

        case c: CDoubleConst => c
        case v: CVar => v
        case _ => return null;
      }
}
