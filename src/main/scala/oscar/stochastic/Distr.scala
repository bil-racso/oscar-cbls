package oscar.stochastic

import JSci.maths.statistics._
import scala.Math._
import scala.util.Random
import scala.collection._

abstract class Distr[A] extends Function0[A] {
  def apply(): A
  def map[B](g: A => B) = new DistrComposition(this, g)
}

abstract class NumericalDistr extends Distr[Double] {

  def min: Double
  def max: Double
  def mean: Double
  def std: Double
}
class DistrComposition[A, B](d: Distr[A], g: A => B) extends Distr[B] {
  def apply() = g(d())
}
class ValueDistr[A](value: A) extends Distr[A] {
  def apply() = value
}

class NumericalValueDistr(value: Double) extends ValueDistr[Double](value) {
  def min = value
  def max = value
  def mean = value
  def std = 0
}

class Flip(p: Double) extends Distr[Boolean] {
  override def apply() = Math.random < p
}

class Distribution(d: ProbabilityDistribution) extends NumericalDistr {
  val generator = new Random()

  def apply(): Double = d.inverse(generator.nextDouble)
  def min = d.min
  def max = d.max
  def std = d.std
  def mean = d.mean
}

class LearnedNumerical(val pmin: Double, val pmax: Double) {

  require(0 <= pmin)
  require(pmin <= 1)
  require(0 <= pmax)
  require(pmax <= 1)

  var current = 0.0
  var tot = 0.0
  var squaredTot = 0.0

  override def equals(o: Any) = {
    val that = o.asInstanceOf[LearnedNumerical]
    pmin == that.pmin && that.pmax == that.pmax &&
      tot == that.tot && that.squaredTot == squaredTot
  }

  def aggregate(o: Any) {

    val that = o.asInstanceOf[LearnedNumerical]

    require(pmin == that.pmin && that.pmax == that.pmax)
    tot += that.tot
    squaredTot += that.squaredTot
  }
  def update(v: Double) {
    current = v
  }
  def observe(v: Double) {
    current = v
    observe
  }
  def observe {
    tot += current
    squaredTot += current * current
    current = 0.0
  }
}

class LearnedNumericalFunction(pmin: Double, pmax: Double) extends Map[Int, LearnedNumerical] {

  var nRea = 0
  val numbers = new mutable.ArrayBuffer[LearnedNumerical]
  def apply(t: Int) = {
    var i = numbers.size - 1
    while (i < t) {
      numbers += new LearnedNumerical(pmin, pmax)
      i += 1
    }
    numbers(t)
  }
  def mean(t: Int) = numbers(t).tot / nRea

  def update(t: Int, v: Double) { this(t).update(v) }
  def observe(f: Traversable[(Int, Double)]) {
    nRea += 1
    for ((t, v) <- f) {
      this(t) observe v
    }
  }
  def observe(f: TableFunction[Int]) {
    nRea += 1
    for ((t, v) <- f) {
      this(t) observe v
    }
  }

  def domain = 0 until numbers.size
}

class LearningFactory {
  val list = new mutable.LinkedList[LearnedNumerical]
  var nRea = 0
  def number = {
    val n = new LearnedNumerical(0.0, 0.0)
    list :+ n
    n
  }
  def observe { nRea += 1 }
  def mean(n: LearnedNumerical) = n.tot / nRea
}

class Choice[A](list: List[(Double, A)]) extends Distr[A] {
  require(list.size > 0)
  require(list.foldLeft(0.0)(_ + _._1) == 1.0)
  def apply(): A = {
    val target = Math.random
    var tot = 0.0
    for (el <- list) {
      tot += el._1
      if (tot > target) return el._2
    }
    assert(false)
    list.first._2
  }
}