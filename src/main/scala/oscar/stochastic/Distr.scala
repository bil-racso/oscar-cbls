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
import scala.math

object Distr {
  private val randomGenerator = new scala.util.Random(4568)
  //def random = randomGenerator
  def apply[A](v: A) = new ValueDistr(v)
}


trait Distr[B] {
  def apply[T](implicit m: DistrSolver[T]): B @cpsParam[Option[T], Option[T]]
  def getNextStochasticRealization(random: scala.util.Random): B
  def map[C](g: B => C) = new MapDistr(this, g)
}

class MapDistr[A,B](d: Distr[A], f: A => B) extends Distr[B]{
  
  def apply[T](implicit m: DistrSolver[T]): B @cpsParam[Option[T], Option[T]] = f(d(m)) 
  def getNextStochasticRealization(random: scala.util.Random): B = f(d.getNextStochasticRealization(random))
}

trait ContinuousDistr[B] extends Distr[B] {
  override def apply[T](implicit m: DistrSolver[T]): B @cpsParam[Option[T], Option[T]] = m.getNextStochasticRealization(this)
  def min: B
  def max: B
  def mean: B
  def std: B
}
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
  require(Math.abs(list.foldLeft(0.0)(_ + _._1) - 1.0) <= 0.0001)
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
  
  override def toString(): String = { list.mkString(", ") }
  
  def getProbability(value : A) : Double = {list.find(pair => pair._2.equals(value)).get._1}
}

object UniformDiscrete {
  def apply(a: Int, b: Int) = new UniformDiscrete(a, b)
}
class UniformDiscrete(val min: Int, val max: Int) extends ContinuousDistr[Int] {
  require(max >= min)
  
  val interval = max-min
  val midInterval = (interval) / 2
  def getNextStochasticRealization(random: scala.util.Random): Int = {
    
    min + random.nextInt(interval)
  }
  def mean = min + midInterval 
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
