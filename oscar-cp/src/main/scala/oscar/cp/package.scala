package oscar

import oscar.cp.core.CPIntVarViewMinus
import oscar.cp.core.CPIntervalVarViewMinus
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPSol
import oscar.cp.modeling.Branchings
import oscar.cp.modeling.Constraints
import oscar.algo.search.Branching
import oscar.algo.search.SearchNode
import oscar.algo.search.SearchStatistics
import oscar.util.selectMin

/**
 * The `cp` package provides useful functionnalities to model problem using
 * the OscaR Constraint Programming Library.
 *
 * === Commonly Used Types ===
 * This package provides type aliases for types which are commonly used,
 * such as the solver and the variables.
 *
 * === Implicit Conversions ===
 * A number of commonly applied implicit conversions are also defined here.
 * Implicit conversions provide additional higher-order function to core classes
 * such as `CPIntVar`, `CPIntervalVar`, or `CPSolver`. Implicit conversion also provide
 * simple and natural modeling functionnalities for sum and element constraints.
 *
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
package object cp extends Constraints with Branchings {

  // Alias to useful classes and companion objects
  type CPIntVar = oscar.cp.core.CPIntVar
  final val CPIntVar = oscar.cp.core.CPIntVar

  type CPIntervalVar = oscar.cp.core.CPIntervalVar
  final val CPIntevalVar = oscar.cp.core.CPIntervalVar

  type CPBoolVar = oscar.cp.core.CPBoolVar
  final val CPBoolVar = oscar.cp.core.CPBoolVar

  type CPSetVar = oscar.cp.core.CPSetVar
  final val CPSetVar = oscar.cp.core.CPSetVar

  type CPGraphVar = oscar.cp.core.CPGraphVar
  final val CPGraphVar = oscar.cp.core.CPGraphVar

  type CPStore = oscar.cp.core.CPStore
  final val CPStore = oscar.cp.core.CPStore

  type CPSolver = oscar.cp.core.CPSolver
  final val CPSolver = oscar.cp.core.CPSolver

  type Constraint = oscar.cp.core.Constraint

  type NoSolutionException = oscar.cp.core.NoSolutionException

  trait CPModel { implicit val solver: CPSolver = CPSolver() }

  /**
   * Filtering power can be specified for some of the constraints.
   * The default filtering is Weak.
   */
  val Strong = CPPropagStrength.Strong
  val Medium = CPPropagStrength.Medium
  val Weak = CPPropagStrength.Weak

  object TightenType extends Enumeration {
    val WeakTighten = Value("weak tighten")
    val StrongTighten = Value("strong tighten")
    val NoTighten = Value("no tighten")
    val MaintainTighten = Value("maintain tighten")
  }
  import TightenType._

  /** Element */

  implicit class ArrayIntElementConstraintBuilder(val a: Array[Int]) extends AnyVal {
    def apply(i: CPIntVar): CPIntVar = element(a, i, Weak)
  }

  implicit class ArrayCPIntVarElementConstraintBuilder(val a: Array[CPIntVar]) extends AnyVal {
    def apply(i: CPIntVar): CPIntVar = elementVar(a, i, Weak)
  }

  implicit class ArrayCPBoolVarElementConstraintBuilder(val a: Array[CPBoolVar]) extends AnyVal {
    def apply(i: CPIntVar): CPIntVar = elementVar(a, i, Weak)
  }

  implicit class IdSeqIntElementConstraintBuilder(val s: IndexedSeq[Int]) extends AnyVal {
    def apply(i: CPIntVar): CPIntVar = element(s, i, Weak)
  }

  implicit class IdSeqCPIntVarElementConstraintBuilder(val s: IndexedSeq[CPIntVar]) extends AnyVal {
    def apply(i: CPIntVar): CPIntVar = elementVar(s, i, Weak)
  }

  implicit class IdSeqCPBoolVarElementConstraintBuilder(val s: IndexedSeq[CPBoolVar]) extends AnyVal {
    def apply(i: CPIntVar): CPIntVar = elementVar(s, i, Weak)
  }

  implicit class ElementIntMatrixConstraintBuilderLine(val a: Array[Array[Int]]) extends AnyVal {
    def apply(i: CPIntVar) = new ElementIntMatrixConstraintBuilderCol(i, a)
  }

  class ElementIntMatrixConstraintBuilderCol(i: CPIntVar, a: Array[Array[Int]]) {
    def apply(j: CPIntVar): CPIntVar = element(a, i, j)
  }

  implicit def convert2(vals: IndexedSeq[Int]) = vals.toArray[Int]

  implicit def indexed2Array(x: IndexedSeq[CPIntVar]) = x.toArray[CPIntVar]
  implicit def args2Array(x: CPIntVar*) = x.toArray[CPIntVar]

  implicit def indexed2ArrayBool(x: IndexedSeq[CPBoolVar]) = x.toArray[CPBoolVar]
  implicit def args2ArrayBool(x: CPBoolVar*) = x.toArray[CPBoolVar]

  //implicit def convertSeqVars2ArrayVars[T <: CPIntVar](x: scala.collection.immutable.IndexedSeq[T]) : Array[T]= x.toArray

  implicit def arrayVar2IterableVarOps(s: Array[CPIntVar]) = new IterableVarOps(s)
  implicit class IterableVarOps(val seq: Iterable[CPIntVar]) extends AnyVal {

    /** @return true is all the variables are bound */
    def areBound: Boolean = seq.forall(_.isBound)

    /** @return one unbound variable with minimum domain (randomly chosen is several of them) */
    def minDomNotBound: CPIntVar = {
      val res: Option[(CPIntVar, Int)] = selectMin(seq.zipWithIndex)(x => !x._1.isBound)(y => (y._1.size, y._2))
      res match {
        case Some((x, i)) => x
        case None         => throw new java.util.NoSuchElementException("no unbound var")
      }
    }

    /** @return the maximum value taken a bound variable or v if no variable is bound */
    def maxBoundOrElse(v: Int): Int = {
      val res: Option[CPIntVar] = selectMin(seq)(_.isBound)(-_.value)
      res match {
        case Some(x) => x.value
        case None    => v
      }
    }
  }

  implicit class CPIntVarOps(x: CPIntVar) {

    /**
     *  Returns the value assigned to the variable.
     *  Throws an Exception if the variable is not assigned.
     */
    def value: Int = {
      if (x.isBound) x.min
      else throw new NoSuchElementException("the variable is not bound")
    }

    /**
     * @return difference between second smallest and smallest value in the domain, Int.MaxInt if variable is bound
     */
    def regret: Int = {
      if (x.isBound) Int.MaxValue
      else {
        val min = x.min
        x.valueAfter(min) - min
      }
    }

    /**
     * @return The median value of the domain of the variable
     */
    def median: Int = {
      val vals = x.toArray.sortBy(i => i)
      vals(vals.length / 2)
    }

    /**
     * -x
     */
    def unary_-() = new CPIntVarViewMinus(x)
    /**
     * x+y
     */
    def +(y: CPIntVar) = plus(x, y)
    /**
     * x-y
     */
    def -(y: CPIntVar) = minus(x, y)
    /**
     * x+y
     */
    def +(y: Int) = plus(x, y)
    /**
     * x-y
     */
    def -(y: Int) = minus(x, y)

    def +(s: String) = s"$x$s"

    /**
     * x*y
     */
    def *(y: CPIntVar): CPIntVar = {
      if (y.isBound) x * (y.value)
      else mul(x, y)
    }
    /**
     * x*y
     */
    def *(y: Int): CPIntVar = mul(x, y)

    def abs = absolute(x)

    /**
     * Reified constraint
     * @param y a variable
     * @return a boolean variable b in the same store linked to x by the relation x == y <=> b == true
     */
    def isEq(y: CPIntVar): CPBoolVar = {
      val b = new CPBoolVar(x.store);
      val ok = x.store.post(new oscar.cp.constraints.EqReifVar(x, y, b));
      assert(ok != CPOutcome.Failure);
      b
    }

  }

  implicit class CPIntervalVarOps(x: CPIntervalVar) {

    /**
     *  Returns the value assigned to the variable.
     *  Throws an Exception if the variable is not assigned.
     */
    def value: Int = {
      if (x.isBound) x.min
      else throw new NoSuchElementException("the variable is not bound")
    }

    /**
     * @return difference between second smallest and smallest value in the domain, Int.MaxInt if variable is bound
     */
    def regret: Int = {
      if (x.isBound) Int.MaxValue
      else {
        val min = x.min
        x.valueAfter(min) - min
      }
    }

    /**
     * @return The median value of the domain of the variable
     */
    def median: Int = {
      val min = x.min
      val max = x.max
      min + ((max - min) / 2)
    }

    /**
     * -x
     */
    def unary_-() = new CPIntervalVarViewMinus(x)
    /**
     * x+y
     */
    def +(y: CPIntVar) = plus(x, y)
    /**
     * x-y
     */
    def -(y: CPIntVar) = minus(x, y)
    /**
     * x+y
     */
    def +(y: Int) = plus(x, y)
    /**
     * x-y
     */
    def -(y: Int) = minus(x, y)

    def +(s: String) = s"$x$s"

    /**
     * x*y
     */
    //def *(y: CPIntVar): CPIntVar = {
    //if (y.isBound) x * (y.value)
    //else mul(x,y)
    //}
    /**
     * x*y
     */
    //def *(y: Int): CPIntVar = mul(x,y)

    //def abs = oscar.cp.modeling.absolute(x)

    /**
     * Reified constraint
     * @param y a variable
     * @return a boolean variable b in the same store linked to x by the relation x == y <=> b == true
     */
    def isEq(y: CPIntervalVar): CPBoolVar = {
      val b = new CPBoolVar(x.store);
      val ok = x.store.post(new oscar.cp.constraints.EqReifIntervalVar(x, y, b));
      assert(ok != CPOutcome.Failure);
      b
    }

  }

  //helper functions

  /**
   * relax randomly k variables in x, others are assigned to the values they have in sol
   */
  def relaxRandomly(x: IndexedSeq[_ <: CPIntVar], sol: CPSol, k: Int): CPOutcome = {
    val cp = x.head.store
    val n = x.size
    val fixed = (0 until n).toSet -- (for (i <- 1 to k) yield scala.util.Random.nextInt(n)).toSet
    cp.post(fixed.map(i => x(i) == sol(x(i))).toArray[Constraint])
  }

  def allBounds(vars: Iterable[_ <: CPIntVar]) = vars.asInstanceOf[Iterable[CPIntVar]].forall(_.isBound)

  // helper functions to define searches

  def minDom(x: CPIntVar): Int = x.size
  def minRegret(x: CPIntVar): Int = x.max - x.min
  def minDomMaxDegree(x: CPIntVar): (Int, Int) = (x.size, -x.constraintDegree)
  def minVar(x: CPIntVar): Int = 1
  def maxDegree(x: CPIntVar): Int = -x.constraintDegree

  def minVal(x: CPIntVar): Int = x.min
  def maxVal(x: CPIntVar): Int = x.max
  def minValminVal(x: CPIntVar): (Int, Int) = (x.min, x.min)

  def branchAssign(variable: CPIntVar, value: Int)(implicit solver: CPSolver): Seq[Alternative] = {
    branch { solver.post(variable == value) } { solver.post(variable != value) }
  }

  // helper functions to model with an implicit CPSolver
  def add(constraints: Iterable[_ <: Constraint])(implicit cp: CPSolver): Unit = cp.add(constraints)

  def add(c: Constraint, propagStrengh: CPPropagStrength)(implicit cp: CPSolver): Unit = cp.add(c, propagStrengh)
  def add(c: Constraint)(implicit cp: CPSolver): Unit = cp.add(c)

  def add(c: CPBoolVar)(implicit cp: CPSolver): Unit = cp.add(c)

  def post(c: Constraint, propagStrengh: CPPropagStrength = Weak)(implicit cp: CPSolver): Unit = cp.post(c, propagStrengh)
  def post(c: Constraint)(implicit cp: CPSolver): Unit = cp.post(c)

  def search(branching: Branching)(implicit cp: CPSolver): SearchNode = cp.search(branching)

  def search(block: => Seq[Alternative])(implicit cp: CPSolver): SearchNode = cp.search(block)

  def minimize(obj: CPIntervalVar)(implicit cp: CPSolver): CPSolver = cp.minimize(obj)
  def maximize(obj: CPIntervalVar)(implicit cp: CPSolver): CPSolver = cp.maximize(obj)

  def onSolution(block: => Unit)(implicit cp: CPSolver): SearchNode = cp.onSolution(block)
  def onSolutionWithStats(block: SearchStatistics => Unit)(implicit cp: CPSolver): SearchNode = cp.onSolutionWithStats(block)

  def start(nSols: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue, maxDiscrepancy: Int = Int.MaxValue)(implicit cp: CPSolver): SearchStatistics = {
    cp.start(nSols, failureLimit, timeLimit, maxDiscrepancy)
  }

  def startSubjectTo(nSols: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue, maxDiscrepancy: Int = Int.MaxValue)(reversibleBlock: => Unit = {})(implicit cp: CPSolver): SearchStatistics = {
    cp.startSubjectTo(nSols, failureLimit, timeLimit, maxDiscrepancy)(reversibleBlock)
  }
}
