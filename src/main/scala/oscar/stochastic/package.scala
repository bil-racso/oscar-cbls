package oscar

import JSci.maths.statistics.ProbabilityDistribution
import org.scala_tools.time.Imports._

object NumericPeriod extends Numeric[Period]{
  
   def plus(x: Period, y: Period): Period = x.plus(y)
  def minus(x: Period, y: Period): Period=x.minus(y)
  def times(x: Period, y: Period): Period=x*y
  def negate(x: Period): Period = -x
  def fromInt(x: Int): Period = new Period(x)
  def toInt(x: Period): Int = x.millis.toInt
  def toLong(x: Period): Long = x.millis
  def toFloat(x: Period): Float = x.millis
  def toDouble(x: Period): Double = x.millis  
  def compare(x: Period, y: Period) = (x.minus(y)).millis
}

package object stochastic {
	implicit val n = NumericPeriod
	implicit def valueToDistr[A](value: A) = new ValueDistr(value)
	//implicit def t2ToPeriod( t: Tuple2[Period,Period]) = UniformDiscrete(t._1,t._2) 
  
	//implicit def probaDistr2Distr(d: ProbabilityDistribution) = new Distribution(d)
}