package oscar.stochastic

import JSci.maths.statistics._
import scala.Math.sqrt
import scala.util.Random
import scala.collection._
import java.util.TreeMap
import collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.util.continuations._
import oscar.cp.constraints.Square

object Distr {
  private val randomGenerator = new scala.util.Random(4568)
  //def random = randomGenerator
  def apply[A](v: A) = new ValueDistr(v)
}

trait Distr[B] {
  def apply[T](implicit m: DistrSolver[T]): B @cpsParam[Option[T], Option[T]]
  def getNextStochasticRealization(random: scala.util.Random): B
  // def map[B](g: A => B) = new DistrComposition(this, g)
}

trait ContinuousDistr[B] extends Distr[B] {
  override def apply[T](implicit m: DistrSolver[T]): B @cpsParam[Option[T], Option[T]] = m.getNextStochasticRealization(this)
  def min: B
  def max: B
  def mean: B
  def std: B
}
//class DistrComposition[A, B](d: Distr[A], g: A => B) extends Distr[B] {
//  def getNextStochasticRealization() = g(d())
//}
class ValueDistr[B](val value: B) extends Distr[B] {
  def getNextStochasticRealization(random: scala.util.Random) = value
  override def apply[T](implicit m: DistrSolver[T]): B @cpsParam[Option[T], Option[T]] = value
}

class NumericalValueDistr(value: Double) extends ValueDistr[Double](value) with ContinuousDistr[Double] {
  override def min = value
  override def max = value
  override def mean = value
  override def std = 0
}

trait DiscreteDistr[B] extends Distr[B] {
  def list: Traversable[(Double, B)]
  def apply[T](implicit m: DistrSolver[T]): B @cpsParam[Option[T], Option[T]] = m.getNextStochasticRealization(this)
}

class Choice[A](val list: List[(Double, A)]) extends DiscreteDistr[A] {
  require(list.size > 0)
  require(list.foldLeft(0.0)(_ + _._1) == 1.0)
  def getNextStochasticRealization(random: scala.util.Random): A = {
    val target = random nextDouble
    var tot = 0.0
    for (el <- list) {
      tot += el._1
      if (tot > target) return el._2
    }
    assert(false)
    list.first._2
  }
}

object UniformDiscrete {
  def apply(a: Long, b: Long) = new UniformDiscrete(a, b)
}
class UniformDiscrete(val min: Long, val max: Long) extends ContinuousDistr[Long] {
  require(max >= min)
  require(max - min < Int.MaxValue)
  val interval = (max - min).toInt
  def getNextStochasticRealization(random: scala.util.Random): Long = {
    min + random.nextInt(interval)
  }
  def mean = min + (max - min) / 2
  def std = { assert(false); -1 }

}

class NumericalChoice(list: List[(Double, Double)])(implicit val op: Operationable[Double]) extends Choice[Double](list) with ContinuousDistr[Double] {
  def mean = (for ((p, v) <- list) yield (op.*(p, v))).sum
  def min = list.map(_._2).min(op)
  def max = list.map(_._2).max(op)
  def std = sqrt(variance)
  def variance = {
    val m = mean
    (for ((p, v) <- list) yield p * (v - m) * (v - m)).sum
  }
  def esperanceSquared = for ((p, v) <- list) yield p * v * v
}

object Flip {
  def apply(p: Double) = new Flip(p)
}
class Flip(p: Double) extends DiscreteDistr[Boolean] {
  override def getNextStochasticRealization(random: scala.util.Random) = random.nextDouble() < p
  override def list = List((p, true), (1 - p, false))
}

class PoissonD(decay: Double) extends ContinuousDistr[Double] {
  val ln = new ExponentialDistribution(decay)
  def min = ln.inverse(0.05)
  val max = ln.inverse(0.95)
  val std = sqrt(ln.getVariance())
  def mean = ln.getMean()
  def getNextStochasticRealization(random: scala.util.Random): Double = ln.inverse(random.nextDouble)

}

class ExponentialD(lambda: Double) extends ContinuousDistr[Double] {
  //val ln = new ExponentialDistribution(decay) 
  private def inverse(p: Double) = {
    require(0 <= p && p <= 1)
    -scala.Math.log(1 - p) / lambda
  }
  def min = inverse(0.05)
  val max = inverse(0.95)
  val std = 1 / (lambda * lambda)
  def mean = 1 / lambda
  def getNextStochasticRealization(random: scala.util.Random): Double = inverse(random.nextDouble)
  //ln.inverse(generator.nextDouble)

}
class LogNormalD(mu: Double, variance: Double) extends ContinuousDistr[Double] {
  val ln = new LognormalDistribution(mu, variance)
  def getNextStochasticRealization(random: scala.util.Random): Double = ln.inverse(random.nextDouble)
  def min = ln.inverse(0.05)
  val max = ln.inverse(0.95)
  val std = ln.getSigmaParameter()
  def mean = ln.getMuParameter()

}

class NormalD(mu: Double, variance: Double) extends ContinuousDistr[Double] {
  val nd = new NormalDistribution(mu, variance)

  def getNextStochasticRealization(random: scala.util.Random): Double = nd.inverse(random.nextDouble)
  def min = nd.inverse(0.05)
  val max = nd.inverse(0.95)
  val std = sqrt(nd.getVariance())
  def mean = nd.getMean()

}

trait Observing {
  def observe{}
}

trait CountNRealizations extends Observing {
  var nRea = 0
  override def observe {
    nRea += 1
  }
}

class AbstractLearnedQuantiles[B](val pmin: Double, val pmax: Double)(implicit op: Operationable[B]) extends LearnedNumerical[B] with Observing {
  var n = 0
  val count = (new TreeMap[B, Int](op))

  require(0 <= pmin)
  require(pmin <= 1)
  require(0 <= pmax)
  require(pmax <= 1)

  override def equals(o: Any) = {
    val that = o.asInstanceOf[AbstractLearnedQuantiles[B]]
    pmin == that.pmin && pmax == that.pmax &&
      super.equals(that)
  }

  override def aggregate(o: Any) {

    val that = o.asInstanceOf[AbstractLearnedQuantiles[B]]

    require(pmin == that.pmin && that.pmax == that.pmax)
    n += that.n
    that.count.foreach(a => count.put(a._1, count.get(a._1) + a._2))
    super.aggregate(that)
  }

  override def observe(v: B) {
    require(op.positive(v))
    super.observe(v)
    if (!op.equiv(op.zero, v)) {
      val o = count.get(v)
      count.put(v, o + 1)
    }
    n += 1
  }

  def quantileUp(p: Double, nRea: Int) = {
    require(p > 0)
    require(p <= 1)
    require(nRea >= n)
    assert(n == count.map(_._2).sum, (n, count))
    val nullObservation = nRea - n
    quantile(Math.ceil(p * nRea).toInt - nullObservation)
  }
  def quantileDown(p: Double, nRea: Int) = {
    require(p > 0)
    require(p <= 1)
    require(nRea >= n)
    assert(n == count.map(_._2).sum, (n, count))
    val nullObservation = nRea - n
    quantile(Math.floor(p * nRea).toInt - nullObservation)
  }
  private def quantile(target: Int): B = {
    if (target <= 0) return op.zero

    require(!count.isEmpty())

    var c = 0
    for (en <- count.entrySet()) {
      c += en.getValue()
      if (c >= target) return en.getKey()
    }
    count.last._1
  }
  override def hasPertinentObservations = super.hasPertinentObservations || count.size != 0
}

class LearnedNumerical[B]()(implicit op: Operationable[B]) extends Observing {

  var current = op.zero
  var tot = op.zero
  var squaredTot = op.zero

  def mean(nRea: Int) = op.*#(tot, 1.0/nRea)
  
  override def equals(o: Any) = {
    val that = o.asInstanceOf[LearnedNumerical[B]]
    tot == that.tot && that.squaredTot == squaredTot
  }

  def aggregate(o: Any) {

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
}

object Learn {

  def number[B](pmin: Double, pmax: Double)(implicit op: Operationable[B]) = new AbstractLearnedQuantiles(pmin, pmax)(op) with CountNRealizations
  def number[B](implicit op: Operationable[B]) = new LearnedNumerical()(op) with CountNRealizations

  def apply[B](pmin: Double, pmax: Double)(implicit op: Operationable[B]) = function(pmin, pmax)(op)
  def apply[B]()(implicit op: Operationable[B]) = function(op)

  def function[B](pmin: Double, pmax: Double)(implicit op: Operationable[B]) = new LearnedNumericalFunctionWithQuantiles(pmin, pmax)
  def function[B](implicit op: Operationable[B]) = new LearnedNumericalFunctionWithMean[B]()
}

class LearnedNumericalFunctionWithQuantiles[B](val pmin: Double, val pmax: Double)(implicit op: Operationable[B]) extends LearnedNumericalFunction[B, AbstractLearnedQuantiles[B]] {

  override def createNumber = new AbstractLearnedQuantiles[B](pmin, pmax)
  def quantileUp(t: Int, d: Double) = this(t) quantileUp (d, nRea)
  def quantileDown(t: Int, d: Double) = this(t) quantileDown (d, nRea)

}

class LearnedNumericalFunctionWithMean[B](implicit op: Operationable[B]) extends LearnedNumericalFunction[B, LearnedNumerical[B]] {
  override def createNumber = new LearnedNumerical[B]()

}

abstract class LearnedNumericalFunction[B, N <: LearnedNumerical[B]](implicit op: Operationable[B]) extends CountNRealizations{

  implicit val numbers = new mutable.ArrayBuffer[N]

  def createNumber: N

  def apply(t: Int) = {
    if (t < 0) throw new ArrayIndexOutOfBoundsException("Do not accept negative indices: " + t)
    var i = numbers.size - 1
    while (i < t) {
      numbers += createNumber
      i += 1
    }
    numbers(t)
  }
  def mean(t: Int) = op./#(numbers(t).tot, nRea)
  def variance(t: Int) = op./#(numbers(t).squaredTot, nRea) - op./#(numbers(t).tot, nRea) * op./#(numbers(t).tot, nRea)
  def std(t: Int) = sqrt(variance(t))

  def update(t: Int, v: B) { this(t).update(v) }

  def aggregate[M <: LearnedNumericalFunction[B, N]](that: M): this.type = {
    nRea += that.nRea
    for ((t, n) <- that) {
      this(t).aggregate(n)
    }
    this
  }
  def +=(that: LearnedNumericalFunction[B, N]) = aggregate(that)
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
