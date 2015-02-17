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