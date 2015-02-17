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

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import JSci.maths.statistics._

/**
 * This class performs a sequential MonteCarlo Simulation on a list of
 * random variables. Instead of using a list to store all the results of
 * runs, it uses an Aggregator object. This sequential simulation uses functional
 * (tail) recursive code instead of sequential imperative code.
 *
 * @param nIterations The number of iterations of simulation (default : 10000)
 * @param nVariables The number of variables handled by each Evaluator
 * @param probabilityDistr The probability distribution associated to random variables (default : Uniform on 0..1)
 * @param typeVariables The type of random variables (default : Double)
 * @param evaluationFunction the function to evaluate on random variates
 * @param resultFunction the function to aggregate the results
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
  // Initialization of random variables.
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
 * This class performs a concurrent MonteCarlo Simulation on a list of
 * random variables. This concurrent simulation is based on scala Futures
 * and Promises, which are not thread-based.
 *
 * @param nIterations The number of iterations of simulation (default : 10000)
 * @param nVariables The number of variables handled by each Evaluator
 * @param probabilityDistr The probability distribution associated to random variables (default : Uniform on 0..1)
 * @param typeVariables The type of random variables (default : Double)
 * @param evaluationFunction the function to evaluate on random variates
 * @param resultFunction the function to aggregate the results
 * @author gustavo.ospina@cetic.be
 */
class MonteCarloConcurrent(nIterations : Long = 10000,
                           nVariables : Int = 1,
                           probabilityDistr : ProbabilityDistribution = new UniformDistribution(0,1),
                           typeVariables : TypeRdVar = DoubleRdVar,
                           evaluationFunction : List[AnyVal] => Double = _ => 0.0,
                           resultFunction : Aggregator => Double = _ => 0.0) {
  private val aggregator = new Aggregator
  private val randomVars = new RandomVarList("RVs", probabilityDistr)
  // Initialization of random variables.
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

  def futureEvals : Future[Boolean] = {
    def futureEvals(i : Long, facc : Future[Boolean]) : Future[Boolean] = {
      if (i < nIterations) {
        val newFacc = facc.map((b) => {
          aggregator + evaluationFunction(randomVars.getValues)
          b
        })
        futureEvals(i+1, newFacc)
      }
      else { facc }
    }
    futureEvals(0, Future { true })
  }

  def runSimulation(): Double = {
    val promiseResult = Promise[Double]()
    val futureResult = promiseResult.future
    aggregator.reset()
    futureEvals onSuccess {
      case b =>
        if (b) { promiseResult success resultFunction(aggregator) }
        else { println("Big Error on concurrent simulation") }
    }
    Await.result(futureResult, Duration.Inf)
  }
}

/**
 * This class performs a sequential MonteCarlo Simulation on a list of
 * random variables. Instead of using a list to store all the results of
 * runs, it uses an aggregator. This sequential simulation uses functional
 * recursive code instead of sequential imperative code with lazy streams,
 * which means that every sampling of random variables is done only when
 * their evaluation is needed.
 *
 * @param nIterations The number of iterations of simulation (default : 10000)
 * @param nVariables The number of variables handled by each Evaluator
 * @param probabilityDistr The probability distribution associated to random variables (default : Uniform on 0..1)
 * @param typeVariables The type of random variables (default : Double)
 * @param evaluationFunction the function to evaluate on random variates
 * @param resultFunction the function to aggregate the results
 * @author gustavo.ospina@cetic.be
 */
class MonteCarloStream(nIterations : Long = 10000,
                       nVariables : Int = 1,
                       probabilityDistr : ProbabilityDistribution = new UniformDistribution(0,1),
                       typeVariables : TypeRdVar = DoubleRdVar,
                       evaluationFunction : List[AnyVal] => Double = _ => 0.0,
                       resultFunction : Aggregator => Double = _ => 0.0) {
  private val aggregator = new Aggregator
  private val randomVars = new RandomVarList("Rvs", probabilityDistr)

  private def streamEvals : Stream[Double] = {
    evaluationFunction(randomVars.getValues) #:: streamEvals
  }
  // Initialization of random variables.
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
    def runSimulation(i : Long, strEv : Stream[Double]) : Double = {
      if (i < nIterations) {
        aggregator + strEv.head
        runSimulation(i+1, strEv.tail)
      }
      else {
        resultFunction(aggregator)
      }
    }
    aggregator.reset()
    runSimulation(0,streamEvals)
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