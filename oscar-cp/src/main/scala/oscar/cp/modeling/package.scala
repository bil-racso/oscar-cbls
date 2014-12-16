/**
 * *****************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 * *****************************************************************************
 */
package oscar.cp

import scala.collection.IterableLike
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom

import oscar.algo.search._
import oscar.cp.constraints._
import oscar.cp.core.CPIntVar
import oscar.cp.core.CPBoolVar
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.util._

/**
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
package object modeling extends Constraints with Branchings {

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

  implicit class CPIntVarOps(val x: CPIntVar) extends AnyVal {

    /**
     * @return the unique value in the domain, None if variable is not bound
     */
    def value: Int = {
      if (x.isBound) x.min
      else throw new NoSuchElementException("the variable is not bound")
    }

    def getValue: Int = value

    /** -x */
    def unary_-() = new CPIntVarViewMinus(x)

    /** x+y */
    def +(y: CPIntVar) = plus(x, y)

    /** x-y */
    def -(y: CPIntVar) = minus(x, y)

    /** x+y */
    def +(y: Int) = plus(x, y)

    /** x-y */
    def -(y: Int) = minus(x, y)

    def +(s: String) = s"$x$s"

    /** x*y */
    def *(y: CPIntVar): CPIntVar = {
      if (y.isBound) x * (y.value)
      else mul(x, y)
    }

    /** x*y */
    def *(y: Int): CPIntVar = mul(x, y)

    def abs = oscar.cp.modeling.absolute(x)

    /**
     * b <=> x == v
     */
    def ===(v: Int) = x.isEq(v)
    /**
     * b <=> x == y
     */
    def ===(y: CPIntVar) = x.isEq(y)
    /**
     * b <=> x!= y
     */
    def !==(y: CPIntVar) = x.isDiff(y)
    /**
     * b <=> x!= y
     */
    def !==(y: Int) = x.isDiff(y)
    /**
     * b <=> x >= y
     */
    def >==(y: Int) = x.isGrEq(y)
    /**
     * b <=> x >= y
     */
    def >==(y: CPIntVar) = x.isGrEq(y)
    /**
     * b <=> x > y
     */
    def >>=(y: Int) = x.isGrEq(y + 1)
    /**
     * b <=> x > y
     */
    def >>=(y: CPIntVar) = {
      val z = y + 1
      x.isGrEq(z)
    }
    /**
     * b <=> x >= y
     */
    def <==(y: Int) = x.isLeEq(y)
    /**
     * b <=> x >= y
     */
    def <==(y: CPIntVar) = y >== x
    /**
     * b <=> x > y
     */
    def <<=(y: Int) = x <== (y - 1)
    /**
     * b <=> x > y
     */
    def <<=(y: CPIntVar) = x <== (y - 1)

    def median: Int = {
      val vals = x.toArray.sorted
      return vals(vals.length / 2)
    }
  }

  implicit class CPIntervalVarOps(val variable: CPIntervalVar) extends AnyVal {

    /**
     * @return the unique value in the domain, None if variable is not bound
     */
    def value: Int = {
      if (variable.isBound) variable.min
      else throw new NoSuchElementException("the variable is not bound")
    }

    def getValue: Int = value

    /**
     * -x
     */
    def unary_-() = new CPIntervalVarViewMinus(variable)
    /**
     * x+y
     */
    def +(that: CPIntervalVar) = plus(variable, that)
    /**
     * x-y
     */
    def -(that: CPIntervalVar) = minus(variable, that)
    /**
     * x+y
     */
    def +(that: Int) = plus(variable, that)
    /**
     * x-y
     */
    def -(that: Int) = minus(variable, that)

    def +(s: String) = s"$variable$s"

    /**
     * Reified constraint
     * @param y a variable
     * @return a boolean variable b in the same store linked to x by the relation x == y <=> b == true
     */
    def isEq(y: CPIntervalVar): CPBoolVar = {
      val b = new CPBoolVar(variable.store);
      val ok = variable.store.post(new oscar.cp.constraints.EqReifIntervalVar(variable, y, b));
      assert(ok != CPOutcome.Failure);
      b
    }

    //  def !=(y: CPIntVar) = new oscar.cp.constraints.DiffVar(this, y)
    /**
     * x!=y
     */
    //  def !=(y: Int) = new oscar.cp.constraints.DiffVal(this, y)
    /**
     * x==y
     */
    def ==(y: CPIntervalVar) = new oscar.cp.constraints.EqInterval(variable, y)
    /**
     * x==y
     */
    def ==(y: Int) = new oscar.cp.constraints.EqVal(variable, y)
    /**
     * x<y
     */
    def <(y: CPIntVar) = new oscar.cp.constraints.Le(variable, y)
    /**
     * x<y
     */
    def <(y: Int) = new oscar.cp.constraints.Le(variable, y)
    /**
     * x>y
     */
    def >(y: CPIntVar) = new oscar.cp.constraints.Gr(variable, y)
    /**
     * x>y
     */
    def >(y: Int) = new oscar.cp.constraints.Gr(variable, y)
    /**
     * x<=y
     */
    def <=(y: CPIntervalVar) = new oscar.cp.constraints.LeEq(variable, y)
    /**
     * x<=y
     */
    def <=(y: Int) = new oscar.cp.constraints.LeEq(variable, y)
    /**
     * x>=y
     */
    def >=(y: CPIntVar) = new oscar.cp.constraints.GrEq(variable, y)
    /**
     * x>=y
     */
    def >=(y: Int) = new oscar.cp.constraints.GrEq(variable, y)
    /**
     * b <=> x == v
     */
    //def ===(v: Int) = this.isEqInterval(v)
    /**
     * b <=> x == y
     */
    //  def ===(y: CPIntVar) = this.isEq(y)
    /**
     * b <=> x!= y
     */
    //  def !==(y: CPIntVar) = this.isDiff(y)
    /**
     * b <=> x!= y
     */
    //  def !==(y: Int) = this.isDiff(y)
    /**
     * b <=> x >= y
     */
    //  def >==(y: Int) = this.isGrEq(y)
    /**
     * b <=> x >= y
     */
    //  def >==(y: CPIntVar) = this.isGrEq(y)
    /**
     * b <=> x > y
     */
    //  def >>=(y: Int) = this.isGrEq(y + 1)
    /**
     * b <=> x > y
     */
    //  def >>=(y: CPIntVar) = this.isGrEq(y + 1)
    /**
     * b <=> x >= y
     */
    //  def <==(y: Int) = this.isLeEq(y)
    /**
     * b <=> x >= y
     */
    //  def <==(y: CPIntVar) = y >== this
    /**
     * b <=> x > y
     */
    //  def <<=(y: Int) = this <== (y - 1)
    /**
     * b <=> x > y
     */
    //  def <<=(y: CPIntVar) = this <== (y - 1)

    /**
     * b <=> x belongs to set
     */
    //  def isIn(set: Set[Int]): CPBoolVar = {
    //    val b = CPBoolVar()(store)
    //    store.post(new InSetReif(this, set, b))
    //    b
    //  }

    //def %(y: Int) = ModuloLHS(this, y)
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

  def minimize(obj: CPIntVar)(implicit cp: CPSolver): CPSolver = cp.minimize(obj)
  def maximize(obj: CPIntVar)(implicit cp: CPSolver): CPSolver = cp.maximize(obj)

  def onSolution(block: => Unit)(implicit cp: CPSolver): SearchNode = cp.onSolution(block)
  def onSolutionWithStats(block: SearchStatistics => Unit)(implicit cp: CPSolver): SearchNode = cp.onSolutionWithStats(block)

  def start(nSols: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue, maxDiscrepancy: Int = Int.MaxValue)(implicit cp: CPSolver): SearchStatistics = {
    cp.start(nSols, failureLimit, timeLimit, maxDiscrepancy)
  }

  def startSubjectTo(nSols: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue, maxDiscrepancy: Int = Int.MaxValue)(reversibleBlock: => Unit = {})(implicit cp: CPSolver): SearchStatistics = {
    cp.startSubjectTo(nSols, failureLimit, timeLimit, maxDiscrepancy)(reversibleBlock)
  }
}
