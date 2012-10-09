package oscar.stochastic

import JSci.maths.statistics._
import scala.Math.sqrt
import scala.util.Random
import scala.collection._
import java.util.TreeMap
import collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.util.continuations._

object Distr{
  private val randomGenerator = new scala.util.Random(4568)
  def random = randomGenerator
  def apply[A](v: A) = new ValueDistr(v)
}

trait Distr[A] {
  def apply[B](implicit m: DistrSolver):A @ cpsParam[Unit,B] 
  def getNextStochasticRealization(): A
 // def map[B](g: A => B) = new DistrComposition(this, g)
}

trait NumericalDistr[B] extends Distr[B] {

  override def apply(implicit m: DistrSolver) = m.getNextStochasticRealizationN(this)
  def min: B
  def max: B
  def mean: B
  def std: B
}
//class DistrComposition[A, B](d: Distr[A], g: A => B) extends Distr[B] {
//  def getNextStochasticRealization() = g(d())
//}
class ValueDistr[A](value: A) extends Distr[A] {
  def getNextStochasticRealization = value
  override def apply(implicit m: DistrSolver) = value
}

class NumericalValueDistr(value: Double) extends ValueDistr[Double](value) with NumericalDistr[Double]{
  def min = value
  def max = value
  def mean = value
  def std = 0
}

trait DiscreteDistr[A] extends Distr[A]{
  def list: Traversable[(Double, A)]
  def apply(implicit m: DistrSolver) = m.getNextStochasticRealization(this)
}


class Choice[A](val list: List[(Double, A)]) extends DiscreteDistr[A] {
  require(list.size > 0)
  require(list.foldLeft(0.0)(_ + _._1) == 1.0)
  def getNextStochasticRealization(): A = {
    val target = Distr.random nextDouble
    var tot = 0.0
    for (el <- list) {
      tot += el._1
      if (tot > target) return el._2
    }
    assert(false)
    list.first._2
  }
}

object UniformDiscrete{
  def apply(a: Long, b: Long) = new UniformDiscrete(a,b)
}
class UniformDiscrete(val min: Long, val max: Long) extends NumericalDistr[Long]{
  require(max >= min)
  require(max-min < Int.MaxValue)
  val interval = (max-min).toInt
  def getNextStochasticRealization():Long = {
    min + Distr.random.nextInt(interval)
  }
  def mean = min + (max-min)/2
  def std = {assert(false);-1}
  
}

class NumericalChoice(list: List[(Double, Double)])(implicit val op: Operationable[Double]) extends Choice[Double](list) with NumericalDistr[Double] {
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


object Flip{
  def apply(p: Double) = new Flip(p)
}
class Flip(p: Double) extends DiscreteDistr[Boolean] {
  override def getNextStochasticRealization() = Math.random < p
  override def list = List( (p,true), (1-p,false))
}

object Distribution{
  def apply(d: ProbabilityDistribution) = new Distribution(d)
}

class Distribution(d: ProbabilityDistribution) extends NumericalDistr[Double] {
  val generator = new Random()

  def getNextStochasticRealization(): Double = d.inverse(generator.nextDouble)
  def min = d.min
  def max = d.max
  def std = d.std
  def mean = d.mean
  
}

class LearnedQuantiles[B](val pmin: Double, val pmax: Double)(implicit op: Operationable[B]) extends LearnedNumerical[B] {
  var n = 0
  val count = (new TreeMap[B, Int](op))

  require(0 <= pmin)
  require(pmin <= 1)
  require(0 <= pmax)
  require(pmax <= 1)

  override def equals(o: Any) = {
    val that = o.asInstanceOf[LearnedQuantiles[B]]
    pmin == that.pmin && pmax == that.pmax &&
      super.equals(that)
  }

  override def aggregate(o: Any) {
	
    val that = o.asInstanceOf[LearnedQuantiles[B]]

    require(pmin == that.pmin && that.pmax == that.pmax)
    n += that.n
    that.count.foreach( a => count.put( a._1, count.get(a._1) + a._2))
    super.aggregate(that)
  }

  override def observe(v: B) {
    require (op.positive(v))
    super.observe(v)
    val o = count.get(v)
    count.put(v, o + 1)
    n += 1
  }

  def quantileUp(p: Double, nRea: Int) = {
    require(p > 0)
    require(p <= 1)
    require (nRea >= n)
    assert(n == count.map(_._2).sum, (n, count))
    val nullObservation = nRea - n
    quantile(Math.ceil(p * nRea).toInt - nullObservation)
  }
  def quantileDown(p: Double, nRea: Int) = {
    require(p > 0)
    require(p <= 1)
    require (nRea >= n)
    assert(n == count.map(_._2).sum, (n, count))
    val nullObservation = nRea - n
    quantile(Math.floor(p * nRea).toInt - nullObservation)
  }
  private def quantile(target: Int): B = {
    if ( target <= 0) return op.zero
    
    require( !count.isEmpty() )
    
    var c = 0
    for (en <- count.entrySet()) {
      c += en.getValue()
      if (c >= target) return en.getKey()
    }
    count.last._1
  }
}

class LearnedNumerical[B](implicit op: Operationable[B]) {

  var current = op.zero
  var tot = op.zero
  var squaredTot = op.zero

  override def equals(o: Any) = {
    val that = o.asInstanceOf[LearnedNumerical[B]]
    tot == that.tot && that.squaredTot == squaredTot
  }

  def aggregate(o: Any) {

    val that = o.asInstanceOf[LearnedNumerical[B]]
    tot = op.+(tot,that.tot)
    squaredTot = op.+(that.squaredTot, squaredTot)
  }
  def update(v: B) {
    current = v
  }
  def apply() = current
  def observe(v: B) {
    tot = op.+(tot,v)
    squaredTot = op.+(squaredTot, op.*(v,v))
    current = op.zero
  }
  def observe {
    observe(current)
  }
}

object Learn{
  def apply[B](pmin: Double, pmax: Double)(implicit op: Operationable[B]) = new LearnedNumericalFunctionWithQuantiles(pmin,pmax)
  def apply[B]()(implicit op: Operationable[B]) = new LearnedNumericalFunctionWithMean[B]()
}

class LearnedNumericalFunctionWithQuantiles[B](pmin: Double, pmax: Double)(implicit op: Operationable[B]) extends LearnedNumericalFunction[B,LearnedQuantiles[B]] {

  override def createNumber = new LearnedQuantiles[B](pmin, pmax)
  def quantileUp(t: Int, d: Double) = this(t) quantileUp (d, nRea)
  def quantileDown(t: Int, d: Double) = this(t) quantileDown (d, nRea)
}

class LearnedNumericalFunctionWithMean[B](implicit op: Operationable[B]) extends LearnedNumericalFunction[B,LearnedNumerical[B]]{
  override def createNumber = new LearnedNumerical[B]()

}

abstract class LearnedNumericalFunction[B,N <: LearnedNumerical[B]](implicit op: Operationable[B]) {
  var nRea = 0
  implicit val numbers = new mutable.ArrayBuffer[N]

  def createNumber:N
  
  def apply(t: Int) = {
	if ( t < 0 ) throw new ArrayIndexOutOfBoundsException("Do not accept negative indices: " + t)
    var i = numbers.size - 1
    while (i < t) {
      numbers += createNumber
      i += 1
    }
    numbers(t)
  }
  def mean(t: Int) = op./#(numbers(t).tot,nRea)
  def variance(t: Int) = op./#(numbers(t).squaredTot,nRea) - op./#(numbers(t).tot,nRea) * op./#(numbers(t).tot,nRea)
  def std(t: Int) = sqrt(variance(t))

  def update(t: Int, v: B) { this(t).update(v) }

  def aggregate[M <: LearnedNumericalFunction[B,N]](that: M):this.type = {
    nRea += that.nRea
    for ((t, n) <- that) {
      this(t).aggregate(n)
    }
    this
  }
  def +=(that: LearnedNumericalFunction[B,N]) = aggregate(that)
  def observe {
    nRea += 1
    for (n <- numbers) n observe
  }
  def observe(f: Traversable[(Int, B)]) {
    nRea += 1
    
    for ((t, v) <- f if t >= 0 ) {
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
