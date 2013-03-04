package oscar.stochastic

import scala.math.sqrt
import scala.collection._
import java.util.TreeMap
import collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.util.continuations._

trait Observing {
  def observe {}
}

trait CountNRealizations extends Observing {
  var nRea = 0
  override def observe {
    nRea += 1
  }
}

object AbstractLearnedQuantiles {
  def apply[B](pmin: Double, pmax: Double)(implicit op: Operationable[B]) =
    new AbstractLearnedQuantiles(pmin, pmax)
  def apply[B](other: AbstractLearnedQuantiles[B])(implicit op: Operationable[B]) = {
    val res = new AbstractLearnedQuantiles[B](other.pmin, other.pmax)
    res.aggregate(other)
    res
  }
  def withCountRealizations[B](pmin: Double, pmax: Double)(implicit op: Operationable[B]) =
    new AbstractLearnedQuantiles(pmin, pmax) with CountNRealizations
}

class AbstractLearnedQuantiles[B](val pmin: Double, val pmax: Double)(implicit op: Operationable[B]) extends LearnedNumerical[B] with Observing with Aggregatable[AbstractLearnedQuantiles[B]] {
  var n: Int = 0
  val count = (new mutable.HashMap[B, Int]()).withDefaultValue(0)
  require(0 <= pmin)
  require(pmin <= 1)
  require(0 <= pmax)
  require(pmax <= 1)

  override def equals(o: Any) = {
    val that = o.asInstanceOf[AbstractLearnedQuantiles[B]]
    pmin == that.pmin && pmax == that.pmax &&
      super.equals(that)
  }

  def aggregate(that: AbstractLearnedQuantiles[B]) = {

    require(pmin == that.pmin && that.pmax == that.pmax)
    n += that.n

    that.count.foreach { a => count(a._1) += a._2 } //.put(a._1, count.get(a._1) + a._2))
    super[LearnedNumerical].aggregate(that)
  }

  override def observe(v: B) {
    //require(op.positive(v))
    super.observe(v)
    if (!op.equiv(op.zero, v)) {
      count(v) += 1
      n += 1
    }
  }

  def quantileUp(p: Double, nRea: Int) = {
    require(p > 0)
    require(p <= 1)
    require(nRea >= n)
    //assert(n == count.map(_._2).sum, (n, count))
    val nullObservation = nRea - n
    quantile(Math.ceil(p * nRea).toInt - nullObservation)
  }
  def quantileDown(p: Double, nRea: Int) = {
    require(p > 0)
    require(p <= 1)
    require(nRea >= n)
    //assert(n == count.map(_._2).sum, (n, count))
    val nullObservation = nRea - n
    quantile(Math.floor(p * nRea).toInt - nullObservation)
  }
  private def quantile(target: Int): B = {
    if (target <= 0) return op.zero

    assume(count.nonEmpty)

    val arr = count.toArray.sorted

    var c = 0
    for (en <- arr) {
      c += en._2
      if (c >= target) return en._1
    }
    count.last._1
  }
  override def hasPertinentObservations = super.hasPertinentObservations || count.size != 0
  override def corresponds(other: LearnedNumerical[B]) = {
    val that = other.asInstanceOf[AbstractLearnedQuantiles[B]]
    super.corresponds(that) && this.pmin == that.pmin && this.pmax == that.pmax && this.count.equals(that.count)
  }
  override def toString() = {
    "Learn.quantile: n: " + n + ", tot: " + tot + ", count: " + count
  }
}

class LearnedNumerical[B]()(implicit op: Operationable[B]) extends Observing with Aggregatable[LearnedNumerical[B]] {

  var current = op.zero
  var tot = op.zero
  var squaredTot = op.zero

  def mean(nRea: Int) = op.*#(tot, 1.0 / nRea)

  override def equals(o: Any) = {
    val that = o.asInstanceOf[LearnedNumerical[B]]
    tot == that.tot && that.squaredTot == squaredTot
  }

  def aggregate(o: LearnedNumerical[B]) {
    val that = o.asInstanceOf[LearnedNumerical[B]]
    tot = op.+(tot, that.tot)
    squaredTot = op.+(that.squaredTot, squaredTot)
  }
  def update(v: B) {
    current = v
  }
  def apply() = current
  def observe(v: B) {
    tot = op.+(tot, v)
    squaredTot = op.+(squaredTot, op.*(v, v))
    current = op.zero
  }
  override def observe {
    super.observe
    observe(current)
  }
  def hasPertinentObservations = tot != 0 || squaredTot != 0
  def corresponds(that: LearnedNumerical[B]) = this.tot == that.tot && this.squaredTot == that.squaredTot
}

trait Learning extends Aggregatable[Learning] {
  type learn = Learning
  var created: List[Aggregatable[_]] = Nil
  def aggregate(that: Learning) = {
    require(that.created.size == this.created.size)
    val iter = created.iterator
    for (n <- created) {
      iter.next.doAggregate(n)
    }
  }
  override def equals(other: Any) = equals(other, false)
  def equals(other: Any, log: Boolean): Boolean = {
    val that = other.asInstanceOf[Learning]
    if (that.created.size != that.created.size) {
      val iter = this.created.iterator
      for (n <- that.created) {
        val thisN = iter.next
        if (n != thisN) {
          return false
        }
      }
      true
    } else true
  }
  def add[T <: Aggregatable[_]](n: T) = {
    created :+= n
    n
  }
  def learnNumber[B](pmin: Double, pmax: Double)(implicit op: Operationable[B]) = add(AbstractLearnedQuantiles.withCountRealizations(pmin, pmax)(op))
  def learnNumber[B](implicit op: Operationable[B]) = add(new LearnedNumerical()(op) with CountNRealizations)

  def apply[B](pmin: Double, pmax: Double)(implicit op: Operationable[B]) = learnFunction(pmin, pmax)(op)
  def apply[B]()(implicit op: Operationable[B]) = learnFunction(op)

  def learnFunction[B](pmin: Double, pmax: Double)(implicit op: Operationable[B]) = add(new LearnedNumericalFunctionWithQuantiles(pmin, pmax))
  def learnFunction[B](implicit op: Operationable[B]) = add(new LearnedNumericalFunctionWithMean[B]())
}

trait Aggregatable[-N] {
  def doAggregate(other: Any) = aggregate(other.asInstanceOf[N])
  def aggregate(that: N): Unit
}
//
//object Learn {
//
//  def number[B](pmin: Double, pmax: Double)(implicit op: Operationable[B]) = AbstractLearnedQuantiles.withCountRealizations(pmin, pmax)(op)
//  def number[B](implicit op: Operationable[B]) = new LearnedNumerical()(op) with CountNRealizations
//
//  def apply[B](pmin: Double, pmax: Double)(implicit op: Operationable[B]) = function(pmin, pmax)(op)
//  def apply[B]()(implicit op: Operationable[B]) = function(op)
//
//  def function[B](pmin: Double, pmax: Double)(implicit op: Operationable[B]) = new LearnedNumericalFunctionWithQuantiles(pmin, pmax)
//  def function[B](implicit op: Operationable[B]) = new LearnedNumericalFunctionWithMean[B]()
//}

class LearnedNumericalFunctionWithQuantiles[B](val pmin: Double, val pmax: Double)(implicit op: Operationable[B]) extends LearnedNumericalFunction[B, AbstractLearnedQuantiles[B]] {

  override def createNumber = AbstractLearnedQuantiles[B](pmin, pmax)
  def quantileUp(t: Int, d: Double) = this(t) quantileUp (d, nRea)
  def quantileDown(t: Int, d: Double) = this(t) quantileDown (d, nRea)

}

class LearnedNumericalFunctionWithMean[B](implicit op: Operationable[B]) extends LearnedNumericalFunction[B, LearnedNumerical[B]] {
  override def createNumber = new LearnedNumerical[B]()

}

abstract class LearnedNumericalFunction[B, N <: LearnedNumerical[B]](implicit op: Operationable[B]) extends CountNRealizations with Aggregatable[LearnedNumericalFunction[B, N]] {

  implicit val numbers = new mutable.ArrayBuffer[N]

  def size = numbers.size
  def createNumber: N

  def firstPertinentIndex = {
    val min = numbers.indexWhere { _.hasPertinentObservations }
    if (min == -1) 0
    else min
  }
  def lastPertinentIndex = numbers.lastIndexWhere { _.hasPertinentObservations }

  def apply(t: Int) = {
    if (t < 0) throw new ArrayIndexOutOfBoundsException("Do not accept negative indices: " + t)
    for (i <- numbers.size to t) {
      numbers += createNumber
    }
    numbers(t)
  }
  def mean(t: Int) = op./#(numbers(t).tot, nRea)
  def variance(t: Int) = op./#(numbers(t).squaredTot, nRea) - op./#(numbers(t).tot, nRea) * op./#(numbers(t).tot, nRea)
  def std(t: Int) = sqrt(variance(t))

  def update(t: Int, v: B) { this(t).update(v) }

  def aggregate(that: LearnedNumericalFunction[B, N]) {
    nRea += that.nRea
    for ((t, n) <- that) {
      this(t).doAggregate(n)
    }
  }
  override def equals(other: Any) = {
    val that = other.asInstanceOf[LearnedNumericalFunction[B,N]]
    that.numbers == this.numbers
  }
  def +=(that: this.type) = aggregate(that)
  override def observe {
    super.observe
    for (n <- numbers) n observe
  }
  def observe(f: Traversable[(Int, B)]) {
    nRea += 1
    for ((t, v) <- f if t >= 0) {
      this(t) observe v
    }
  }
  //  def observe(f: TableFunction[B]) {
  //    nRea += 1
  //    for (((t, v)) <- f) {
  //      this(t) observe v
  //    }
  //  }

  def domain = 0 until numbers.size
  def foreach[U](f: ((Int, N)) => U) = (for (t <- domain) yield (t, numbers(t))).foreach(f)
  def filter(f: ((Int, N)) => Boolean) = (for (t <- domain) yield (t, numbers(t))).filter(f)

}
//
//class LearningFactory[B] {
//  val list = new mutable.LinkedList[LearnedNumerical[B]]
//  var nRea = 0
//  def number = {
//    val n = new LearnedNumerical()
//    list :+ n
//    n
//  }
//  def observe { nRea += 1 }
//  def mean(n: LearnedNumerical) = n.tot / nRea
//}
