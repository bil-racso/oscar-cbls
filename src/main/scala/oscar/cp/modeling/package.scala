package oscar.cp

import scala.util.continuations._
import scala.collection.IterableLike
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom

import oscar.search._
import oscar.cp.constraints._
import oscar.cp.core.CPVarInt
import oscar.cp.core.CPVarBool
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.util._

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
package object modeling extends Constraints {

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
  }
  import TightenType._

  /**
   * Element
   *
   */

  implicit def array2ElementConstraintBuilder(a: Array[Int]) = new ArrayIntElementConstraintBuilder(a)
  implicit def array2ElementConstraintBuilder(a: Array[CPVarInt]) = new ArrayCPVarIntElementConstraintBuilder(a)
  implicit def array2ElementConstraintBuilder(a: Array[CPVarBool]) = new ArrayCPVarBoolElementConstraintBuilder(a)

  implicit def idSeq2ElementConstraintBuilder(s: IndexedSeq[Int]) = new IdSeqIntElementConstraintBuilder(s)
  implicit def idSeq2ElementConstraintBuilder(s: IndexedSeq[CPVarInt]) = new IdSeqCPVarIntElementConstraintBuilder(s)
  implicit def idSeq2ElementConstraintBuilder(s: IndexedSeq[CPVarBool]) = new IdSeqCPVarBoolElementConstraintBuilder(s)

  implicit def matrix2ElementConstraintBuilder(a: Array[Array[Int]]) = new ElementIntMatrixConstraintBuilderLine(a)

  abstract class ElementConstraintBuilder {
    def apply(i: CPVarInt): CPVarInt
  }

  class ArrayIntElementConstraintBuilder(a: Array[Int]) extends ElementConstraintBuilder {
    override def apply(i: CPVarInt): CPVarInt = element(a, i,Weak)
  }

  class ArrayCPVarIntElementConstraintBuilder(a: Array[CPVarInt]) extends ElementConstraintBuilder {
    override def apply(i: CPVarInt): CPVarInt = elementVar(a, i,Weak)
  }

  class ArrayCPVarBoolElementConstraintBuilder(a: Array[CPVarBool]) extends ElementConstraintBuilder {
    override def apply(i: CPVarInt): CPVarInt = elementVar(a, i,Weak)
  }

  class IdSeqIntElementConstraintBuilder(s: IndexedSeq[Int]) extends ElementConstraintBuilder {
    override def apply(i: CPVarInt): CPVarInt = element(s, i,Weak)
  }

  class IdSeqCPVarIntElementConstraintBuilder(s: IndexedSeq[CPVarInt]) extends ElementConstraintBuilder {
    override def apply(i: CPVarInt): CPVarInt = elementVar(s, i,Weak)
  }

  class IdSeqCPVarBoolElementConstraintBuilder(s: IndexedSeq[CPVarBool]) extends ElementConstraintBuilder {
    override def apply(i: CPVarInt): CPVarInt = elementVar(s, i,Weak)
  }

  class ElementIntMatrixConstraintBuilderLine(a: Array[Array[Int]]) {
    def apply(i: CPVarInt) = new ElementIntMatrixConstraintBuilderCol(i, a)
  }

  class ElementIntMatrixConstraintBuilderCol(i: CPVarInt, a: Array[Array[Int]]) {
    def apply(j: CPVarInt): CPVarInt = element(a, i, j)
  }

  class CPVarBoolWrappper(val b: oscar.cp.core.CPVarBool) {
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
  implicit def convert2CPVarBoolWrapper(b: CPVarBool) = new CPVarBoolWrappper(b)

  class CPVarIntWrappper(val x: CPVarInt) {
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
    def !=(y: CPVarInt) = new Diff(x, y)
    /**
     * x!=y
     */
    def !=(y: Int) = new Diff(x, y)
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
  implicit def convert2CPVarIntWrapper(x: CPVarInt) = new CPVarIntWrappper(x)

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

  implicit def cpVarSeq2EnrichedCPVarSeq(s: Iterable[CPVarInt]) = new EnrichedCPVarSeq(s)

  implicit def cpVarArray2EnrichedCPVarSeq(s: Array[CPVarInt]) = new EnrichedCPVarSeq(s)

  class EnrichedCPVarSeq(val seq: Iterable[CPVarInt]) {
    /**
     * @return one unbound variable with minimum domain (randomly chosen is several of them)
     */
    def minDomNotBound: CPVarInt = {
      val res: Option[(CPVarInt, Int)] = selectMin(seq.zipWithIndex)(x => !x._1.isBound)(y => (y._1.size, y._2))
      res match {
        case Some((x, i)) => x
        case None => throw new java.util.NoSuchElementException("no unbound var")
      }
    }

    /**
     * @return the maximum value taken a bound variable or v if no variable is bound
     */
    def maxBoundOrElse(v: Int): Int = {
      val res: Option[CPVarInt] = selectMin(seq)(_.isBound)(-_.value)
      res match {
        case Some(x) => x.value
        case None => v
      }
    }

  }

  //helper functions

  def allBounds(vars: Iterable[CPVarInt]) = vars.forall(_.isBound)

  def argMax[A](indexes: Iterable[A])(f: A => Int): Iterable[A] = {
    val max = indexes.map(f).max
    indexes.filter(f(_) == max)
  }

  def argMax2[A](indexes: Iterable[A])(f1: A => Int, f2: A => Int): Iterable[A] = {
    val maxf1 = indexes.map(f1).max
    val tmp = indexes.filter(f1(_) == maxf1)
    val maxf2 = tmp.map(f2).max
    tmp.filter(f2(_) == maxf2)
  }

  def argMin[A](indexes: Iterable[A])(f: A => Int): Iterable[A] = {
    val min = indexes.map(f).min
    indexes.filter(f(_) == min)
  }

}
