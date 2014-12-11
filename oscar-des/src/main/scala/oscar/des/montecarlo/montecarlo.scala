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
 * @param probDist The probability distribution associated to random variables (default : Uniform on 0..1)
 * @param typeVars The type of random variables (default : Double)
 * @author gustavo.ospina@cetic.be
 */
class MonteCarloSequential(nIterations : Long = 10000,
                           nVariables : Int = 1,
                           probDist : ProbabilityDistribution = new UniformDistribution(0,1),
                           typeVars : TypeRdVar = DoubleRdVar) {
  private var evaluationFunc: List[AnyVal] => Double = (rvl) => 0.0
  private var resultFunc: Aggregator => Double = (agg) => 0.0
  private val aggregator = new Aggregator
  private val randomVars = new RandomVarList(probDist)
  // Initialization of random variables.
  initRandomVars()

  def initRandomVars(): Unit = {
    def initRandomVars(i : Long): Unit = {
      if (i < nVariables) {
        typeVars match {
          case IntRdVar => randomVars.addIntRandomVar("IntRdV" + i)
          case DoubleRdVar => randomVars.addDoubleRandomVar("DoubleRdV" + i)
        }
        initRandomVars(i+1)
      }
    }
    initRandomVars(0)
  }

  def setEvaluationFunction(ef : List[AnyVal] => Double): Unit = {
    evaluationFunc = ef
  }

  def setResultFunction(rf : Aggregator => Double): Unit = {
    resultFunc = rf
  }

  def runSimulation(): Double = {
    def runSimulation(i : Long) : Double = {
      if (i < nIterations) {
        randomVars.setValues()
        aggregator + evaluationFunc(randomVars.getValues)
        runSimulation(i+1)
      }
      else {
        resultFunc(aggregator)
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
 * @param probDist The probability distribution associated to random variables (default : Uniform on 0..1)
 * @param typeVars The type of random variables (default : Double)
 * @author gustavo.ospina@cetic.be
 */
class MonteCarloConcurrent(nIterations : Long = 10000,
                           nVariables : Int = 1,
                           probDist : ProbabilityDistribution = new UniformDistribution(0,1),
                           typeVars : TypeRdVar = DoubleRdVar) {
  private var evaluationFunc: List[AnyVal] => Double = (rvl) => 0.0
  private var resultFunc: Aggregator => Double = (agg) => 0.0
  private val aggregator = new Aggregator
  private val randomVars = new RandomVarList(probDist)
  // Initialization of random variables.
  initRandomVars()

  def initRandomVars(): Unit = {
    def initRandomVars(i : Long): Unit = {
      if (i < nVariables) {
        typeVars match {
          case IntRdVar => randomVars.addIntRandomVar("IntRdV" + i)
          case DoubleRdVar => randomVars.addDoubleRandomVar("DoubleRdV" + i)
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
          randomVars.setValues()
          aggregator + evaluationFunc(randomVars.getValues)
          b
        })
        futureEvals(i+1, newFacc)
      }
      else { facc }
    }
    futureEvals(0, Future { true })
  }

  def setEvaluationFunction(ef : List[AnyVal] => Double): Unit = {
    evaluationFunc = ef
  }

  def setResultFunction(rf : Aggregator => Double): Unit = {
    resultFunc = rf
  }

  def runSimulation(): Double = {
    val promiseResult = Promise[Double]()
    val futureResult = promiseResult.future
    aggregator.reset()
    futureEvals onSuccess {
      case b =>
        if (b) { promiseResult success resultFunc(aggregator) }
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
 * @param probDist The probability distribution associated to random variables (default : Uniform on 0..1)
 * @param typeVars The type of random variables (default : Double)
 * @author gustavo.ospina@cetic.be
 */
class MonteCarloStream(nIterations : Long = 10000,
                       nVariables : Int = 1,
                       probDist : ProbabilityDistribution = new UniformDistribution(0,1),
                       typeVars : TypeRdVar = DoubleRdVar) {
  private var evaluationFunc: List[AnyVal] => Double = (rvl) => 0.0
  private var resultFunc: Aggregator => Double = (agg) => 0.0
  private val aggregator = new Aggregator
  private val randomVars = new RandomVarList(probDist)

  private def streamEvals : Stream[Double] = {
    randomVars.setValues()
    evaluationFunc(randomVars.getValues) #:: streamEvals
  }
  // Initialization of random variables.
  initRandomVars()

  def initRandomVars(): Unit = {
    def initRandomVars(i : Long): Unit = {
      if (i < nVariables) {
        typeVars match {
          case IntRdVar => randomVars.addIntRandomVar("IntRdV" + i)
          case DoubleRdVar => randomVars.addDoubleRandomVar("DoubleRdV" + i)
        }
        initRandomVars(i+1)
      }
    }
    initRandomVars(0)
  }

  def setEvaluationFunction(ef : List[AnyVal] => Double): Unit = {
    evaluationFunc = ef
  }

  def setResultFunction(rf : Aggregator => Double): Unit = {
    resultFunc = rf
  }

  def runSimulation(): Double = {
    def runSimulation(i : Long, strEv : Stream[Double]) : Double = {
      if (i < nIterations) {
        aggregator + strEv.head
        runSimulation(i+1, strEv.tail)
      }
      else {
        resultFunc(aggregator)
      }
    }
    aggregator.reset()
    runSimulation(0,streamEvals)
  }
}

/**
 * This class represents a very simple statistical aggregator. For the moment,
 * it contains just the aggregated sum and multiplication.
 */
class Aggregator{
  var sum = 0.0
  var mult = 1.0
  var size : Long = 1

  def +(x : Double): Unit = {
    sum += x
    mult *= x
    size += 1
  }

  def reset(): Unit = {
    sum = 0.0
    mult = 1.0
    size = 0
  }

  override def toString = s"Aggregator(sum=$sum, mult=$mult, size=$size"
}