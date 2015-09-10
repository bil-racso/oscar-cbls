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
  ******************************************************************************/

package oscar.des.montecarlo

import JSci.maths.statistics._
import scala.collection.immutable.SortedMap

/**
 * Monte Carlo framework.
 *
 * @param nIterations the number of iterations
 * @param nVariables the number of random variables
 * @param probabilityDistr the probability distribution of all variables
 * @param typeVariables type of values (Int, Double)
 * @param evaluationFunction function to evaluate on random variables in each
 *                           iteration
 * @param resultFunction function to analyse the aggregated results
 * @author gustavo.ospina@cetic.be
 */
class MonteCarlo(nIterations : Long = 10000,
                 nVariables : Int = 1,
                 probabilityDistr : ProbabilityDistribution = new UniformDistribution(0,1),
                 typeVariables : TypeRdVar = DoubleRdVar,
                 evaluationFunction : List[AnyVal] => Double = _ => 0.0,
                 resultFunction : Aggregator => Double = _ => 0.0) {
  private val aggregator = new Aggregator
  private val randomVars = new RandomVarList("RVs", probabilityDistr)
  initRandomVars()

  def initRandomVars(): Unit = {
    def initRandomVars(i : Long): Unit = {
      if (i < nVariables) {
        typeVariables match {
          case IntRdVar => randomVars.addIntRandomVar()
          case DoubleRdVar => randomVars.addDoubleRandomVar()
        }
        initRandomVars(i+1)
      }
    }
    initRandomVars(0)
  }

  def runSimulation(): Double = {
    def runSimulation(i : Long) : Double = {
      if (i < nIterations) {
        aggregator + evaluationFunction(randomVars.getValues)
        runSimulation(i+1)
      }
      else {
        resultFunction(aggregator)
      }
    }
    aggregator.reset()
    runSimulation(0)
  }
}

/**
 * This class represents a very very simple statistical aggregator.
 */
class Aggregator{
  var sum = 0.0
  var mult = 1.0
  var size = 1L
  var max = Double.NegativeInfinity
  var min = Double.PositiveInfinity

  def +(x : Double): Unit = {
    sum += x
    mult *= x
    size += 1
    if (x > max)
      max = x
    if (x < min)
      min = x
  }

  def reset(): Unit = {
    sum = 0.0
    mult = 1.0
    size = 0
    max = Double.NegativeInfinity
    min = Double.PositiveInfinity
  }

  override def toString = s"Aggregator(sum=$sum, mult=$mult, size=$size, max=$max, min=$min)"
}


class RichAggregator(fields:Array[String]){
  val fieldDico:SortedMap[String,Int] =
    SortedMap.empty[String,Int].++(for (a <- fields.indices) yield (fields(a),a))

  var recordedData:List[Array[Double]] = List.empty

  def addData(d:Array[Double]): Unit ={
    recordedData = d :: recordedData
  }

  def statisticsForField(field:String):Statistics = {
    val id = fieldDico(field)
    val dataForThisField = recordedData.map(_(id))
    Statistics(dataForThisField)
  }

  def dataForTheseTwoFields(field1:String,field2:String):List[(Double,Double)] = {
    val id1 = fieldDico(field1)
    val id2 = fieldDico(field2)
    recordedData.map(line => (line(id1),line(id2)))
  }
}

case class Statistics(min:Double, max:Double, avg:Double, med:Double){
  override def toString: String = "(min:" + min + " max:" + max + " avg:" + avg + " med:" + med + ")"
  def denseString:String = padToLength("" + min,4) + " " + padToLength("" + max,4) + " " + padToLength("" + avg,4) + " " + padToLength("" + med,5)
  def nSpace(n:Int):String = if(n <= 0) "" else " " + nSpace(n-1)
  private def padToLength(s: String, l: Int) = (s + nSpace(l)).substring(0, l)
}

object Statistics {
  def apply(l: List[Double]): Statistics = {
    require(l.nonEmpty)
    val sorted = l.sorted
    val size = l.size
    Statistics(min=sorted.head, max = sorted.last, avg=l.sum/size, med=sorted.apply(size/2))
  }
}

