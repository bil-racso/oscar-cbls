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
 *******************************************************************************/
package oscar.cp

import scala.util.continuations._
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

  implicit def arrayVar2IterableVarOps(s: Array[CPIntVar]) = new IterableVarOps(s)
  implicit class IterableVarOps(val seq: Iterable[CPIntVar]) extends AnyVal {

    /** @return true is all the variables are bound */
    def areBound: Boolean = seq.forall(_.isBound)

    /** @return one unbound variable with minimum domain (randomly chosen is several of them) */
    def minDomNotBound: CPIntVar = {
      val res: Option[(CPIntVar, Int)] = selectMin(seq.zipWithIndex)(x => !x._1.isBound)(y => (y._1.size, y._2))
      res match {
        case Some((x, i)) => x
        case None => throw new java.util.NoSuchElementException("no unbound var")
      }
    }

    /** @return the maximum value taken a bound variable or v if no variable is bound */
    def maxBoundOrElse(v: Int): Int = {
      val res: Option[CPIntVar] = selectMin(seq)(_.isBound)(-_.value)
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
  def relaxRandomly(x: IndexedSeq[_ <: CPIntVar], sol: CPSol, k: Int): CPOutcome = {
    val cp = x.head.s
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

  // helper functions to model with an implicit CPSolver
  def add(constraints: Iterable[_ <: Constraint])(implicit cp: CPSolver): Unit = cp.add(constraints)
  def add(c: Constraint, propagStrengh: CPPropagStrength = Weak)(implicit cp: CPSolver): Unit = cp.add(c, propagStrengh)
  def add(c: CPBoolVar)(implicit cp: CPSolver): Unit = cp.add(c)
  def post(c: Constraint, propagStrengh: CPPropagStrength = Weak)(implicit cp: CPSolver): Unit = cp.post(c, propagStrengh)
  def search(branching: Branching)(implicit cp: CPSolver): SearchNode = cp.search(branching)
  def search(block: => Seq[Alternative])(implicit cp: CPSolver): SearchNode = cp.search(block)
  def minimize(obj: CPIntVar)(implicit cp: CPSolver): CPSolver = cp.minimize(obj)
  def maximize(obj: CPIntVar)(implicit cp: CPSolver): CPSolver = cp.maximize(obj)
  def onSolution(block: => Unit)(implicit cp: CPSolver): SearchNode = cp.onSolution(block)
  def start(nSols: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue, maxDiscrepancy: Int = Int.MaxValue)(implicit cp: CPSolver): SearchStatistics = {
    cp.start(nSols, failureLimit, timeLimit, maxDiscrepancy)
  }
  def startSubjectTo(nSols: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue, maxDiscrepancy: Int = Int.MaxValue)(reversibleBlock: => Unit = {})(implicit cp: CPSolver): SearchStatistics = {
    cp.startSubjectTo(nSols, failureLimit, timeLimit, maxDiscrepancy)(reversibleBlock)
  }
}
