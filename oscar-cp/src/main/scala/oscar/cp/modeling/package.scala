/*******************************************************************************
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
 * *****************************************************************************/
package oscar.cp

import scala.util.continuations._
import scala.collection.IterableLike
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom

import oscar.algo.search._
import oscar.cp.constraints._
import oscar.cp.core.CPVarInt
import oscar.cp.core.CPVarBool
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
    def apply(i: CPVarInt): CPVarInt = element(a, i, Weak)
  }

  implicit class ArrayCPVarIntElementConstraintBuilder(val a: Array[CPVarInt]) extends AnyVal {
    def apply(i: CPVarInt): CPVarInt = elementVar(a, i, Weak)
  }

  implicit class ArrayCPVarBoolElementConstraintBuilder(val a: Array[CPVarBool]) extends AnyVal {
    def apply(i: CPVarInt): CPVarInt = elementVar(a, i, Weak)
  }

  implicit class IdSeqIntElementConstraintBuilder(val s: IndexedSeq[Int]) extends AnyVal {
    def apply(i: CPVarInt): CPVarInt = element(s, i, Weak)
  }

  implicit class IdSeqCPVarIntElementConstraintBuilder(val s: IndexedSeq[CPVarInt]) extends AnyVal {
    def apply(i: CPVarInt): CPVarInt = elementVar(s, i, Weak)
  }

  implicit class IdSeqCPVarBoolElementConstraintBuilder(val s: IndexedSeq[CPVarBool]) extends AnyVal {
    def apply(i: CPVarInt): CPVarInt = elementVar(s, i, Weak)
  }

  implicit class ElementIntMatrixConstraintBuilderLine(val a: Array[Array[Int]]) extends AnyVal {
    def apply(i: CPVarInt) = new ElementIntMatrixConstraintBuilderCol(i, a)
  }

  class ElementIntMatrixConstraintBuilderCol(i: CPVarInt, a: Array[Array[Int]]) {
    def apply(j: CPVarInt): CPVarInt = element(a, i, j)
  }

  implicit class CPVarBoolWrappper(val b: oscar.cp.core.CPVarBool) extends AnyVal {
    /**
     * -b
     */
    def unary_!() = b.not()
    /**
     * x | y
     */
    def |(y: CPVarBool) = b.or(y)
    /**
     * x || y
     */
    def ||(y: CPVarBool) = b.or(y)
    /**
     * x & y
     */
    def &(y: CPVarBool) = b.and(y)
    /**
     * x && y
     */
    def &&(y: CPVarBool) = b.and(y)
    /**
     * x ==> y
     */
    def ==>(y: CPVarBool) = b.implies(y)
    /**
     * x != y
     */
    def !=(y: CPVarBool) = new Not(b, y)
  }

  implicit class CPVarIntWrappper(val x: CPVarInt) extends AnyVal {
    /**
     * -x
     */
    def unary_-() = x.opposite()
    /**
     * x+y
     */
    def +(y: CPVarInt) = x.plus(y)
    /**
     * x-y
     */
    def -(y: CPVarInt) = x.minus(y)
    /**
     * x+y
     */
    def +(y: Int) = x.plus(y)
    /**
     * x-y
     */
    def -(y: Int) = x.minus(y)
    /**
     * x*y
     */
    def *(y: CPVarInt) = x.mul(y)
    /**
     * x*y
     */
    def *(y: Int) = x.mul(y)
    /**
     * x!=y
     */
    def !=(y: CPVarInt) = new DiffVar(x, y)
    /**
     * x!=y
     */
    def !=(y: Int) = new DiffVal(x, y)
    /**
     * x==y
     */
    def ==(y: CPVarInt) = new Eq(x, y)
    /**
     * x==y
     */
    def ==(y: Int) = new Eq(x, y)
    /**
     * x<y
     */
    def <(y: CPVarInt) = new Le(x, y)
    /**
     * x<y
     */
    def <(y: Int) = new Le(x, y)
    /**
     * x>y
     */
    def >(y: CPVarInt) = new Gr(x, y)
    /**
     * x>y
     */
    def >(y: Int) = new Gr(x, y)
    /**
     * x<=y
     */
    def <=(y: CPVarInt) = new LeEq(x, y)
    /**
     * x<=y
     */
    def <=(y: Int) = new LeEq(x, y)
    /**
     * x>=y
     */
    def >=(y: CPVarInt) = new GrEq(x, y)
    /**
     * x>=y
     */
    def >=(y: Int) = new GrEq(x, y)
    /**
     * b <=> x == v
     */
    def ===(v: Int) = x.isEq(v)
    /**
     * b <=> x == y
     */
    def ===(y: CPVarInt) = x.isEq(y)
    /**
     * b <=> x!= y
     */
    def !==(y: CPVarInt) = x.isDiff(y)
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
    def >==(y: CPVarInt) = x.isGrEq(y)
    /**
     * b <=> x > y
     */
    def >>=(y: Int) = x.isGrEq(y + 1)
    /**
     * b <=> x > y
     */
    def >>=(y: CPVarInt) = x.isGrEq(y + 1)
    /**
     * b <=> x >= y
     */
    def <==(y: Int) = x.isLeEq(y)
    /**
     * b <=> x >= y
     */
    def <==(y: CPVarInt) = y >== x
    /**
     * b <=> x > y
     */
    def <<=(y: Int) = x <== (y - 1)
    /**
     * b <=> x > y
     */
    def <<=(y: CPVarInt) = x <== (y - 1)
  }

  implicit def convert2(vals: IndexedSeq[Int]) = vals.toArray[Int]

  implicit def indexed2Array(x: IndexedSeq[CPVarInt]) = x.toArray[CPVarInt]
  implicit def args2Array(x: CPVarInt*) = x.toArray[CPVarInt]

  implicit def indexed2ArrayBool(x: IndexedSeq[CPVarBool]) = x.toArray[CPVarBool]
  implicit def args2ArrayBool(x: CPVarBool*) = x.toArray[CPVarBool]

  //implicit def convertSeqVars2ArrayVars[T <: CPVarInt](x: scala.collection.immutable.IndexedSeq[T]) : Array[T]= x.toArray

  implicit def richIterable[A, Repr](xs: SeqLike[A, Repr]) = new {
    def suspendable = new {
      def foreach(yld: A => Unit @suspendable): Unit @suspendable = {
        loop(xs.indices) {
          i => yld(xs(i))
        }
      }
    }
  }

  def loopWhile[T](cond: => Boolean)(body: => (Unit @suspendable)): Unit @suspendable = {
    if (cond) {
      body
      loopWhile[T](cond)(body)
    }
  }

  def loop(r: Range)(body: Int => (Unit @suspendable)): Unit @suspendable = {
    var i = r.start
    loopWhile(i < r.end) {
      val k = i
      body(i)
      i = k + 1
    }
  }

  implicit def arrayVar2IterableVarOps(s: Array[CPVarInt]) = new IterableVarOps(s)
  implicit class IterableVarOps(val seq: Iterable[CPVarInt]) extends AnyVal {

    /** @return true is all the variables are bound */
    def areBound: Boolean = seq.forall(_.isBound)

    /** @return one unbound variable with minimum domain (randomly chosen is several of them) */
    def minDomNotBound: CPVarInt = {
      val res: Option[(CPVarInt, Int)] = selectMin(seq.zipWithIndex)(x => !x._1.isBound)(y => (y._1.size, y._2))
      res match {
        case Some((x, i)) => x
        case None => throw new java.util.NoSuchElementException("no unbound var")
      }
    }

    /** @return the maximum value taken a bound variable or v if no variable is bound */
    def maxBoundOrElse(v: Int): Int = {
      val res: Option[CPVarInt] = selectMin(seq)(_.isBound)(-_.value)
      res match {
        case Some(x) => x.value
        case None => v
      }
    }
  }

  //helper functions

  /**
   * relax randomly k variables in x, others are assigned to the values they have in sol
   */
  def relaxRandomly(x: IndexedSeq[_ <: CPVarInt], sol: CPSol, k: Int): CPOutcome = {
    val cp = x.head.s
    val n = x.size
    val fixed = (0 until n).toSet -- (for (i <- 1 to k) yield scala.util.Random.nextInt(n)).toSet
    cp.post(fixed.map(i => x(i) == sol(x(i))).toArray[Constraint])
  }

  def allBounds(vars: Iterable[_ <: CPVarInt]) = vars.asInstanceOf[Iterable[CPVarInt]].forall(_.isBound)

  // helper functions to define searches

  def minDom(x: CPVarInt): Int = x.size
  def minRegret(x: CPVarInt): Int = x.max - x.min
  def minDomMaxDegree(x: CPVarInt): (Int, Int) = (x.size, -x.constraintDegree)
  def minVar(x: CPVarInt): Int = 1
  def maxDegree(x: CPVarInt): Int = -x.constraintDegree

  def minVal(x: CPVarInt): Int = x.min
  def maxVal(x: CPVarInt): Int = x.max
  def minValminVal(x: CPVarInt): (Int, Int) = (x.min, x.min)

  // helper functions to model with an implicit CPSolver
  def add(c: Constraint, propagStrengh: CPPropagStrength = Weak)(implicit cp: CPSolver): Unit = cp.add(c)
  def solve(implicit cp: CPSolver): CPSolver = cp.solve() subjectTo ()
  def minimize(obj: CPVarInt)(implicit cp: CPSolver): CPSolver = cp.minimize(obj) subjectTo ()
  def maximize(obj: CPVarInt)(implicit cp: CPSolver): CPSolver = cp.maximize(obj) subjectTo ()
  def onSolution(block: => Unit)(implicit cp: CPSolver): SearchNode = cp.onSolution(block)
  def start(nbSolMax: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue, maxDiscrepancy: Int = Int.MaxValue)(implicit cp: CPSolver): SearchStatistics = {
    cp.start(nbSolMax, failureLimit, timeLimit, maxDiscrepancy)
  }
}
