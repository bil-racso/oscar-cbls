package oscar.stochastic

import oscar.stochastic._
import scala.util.continuations._

trait CanBeMultipliedWithDouble{
  def *(d: Double): this.type
}

abstract class DistrSolver {

  def getNextStochasticRealizationN[B](distr: NumericalDistr[B]): B
  //def apply[B](distr: Choice[B]): B
  
  def getNextStochasticRealization[A,B](ch: DiscreteDistr[B]): B @ cpsParam[Unit,A]
}

class StochasticSolver extends DistrSolver{
  def getNextStochasticRealizationN[B](distr: NumericalDistr[B]) = distr.getNextStochasticRealization()
  
  def getNextStochasticRealization[A,B](ch: DiscreteDistr[B]) = ch.getNextStochasticRealization()  
}

class EsperanceSolver extends DistrSolver{
  def getNextStochasticRealizationN[B](distr: NumericalDistr[B]) = distr.mean
		  
  def getNextStochasticRealization[A,B](ch: DiscreteDistr[B])(implicit op: Operationable[B]) = {
	  shift { k: (B => A) => ch.list.foldLeft(op.zero){ (a,b) => op.+(a,op.**(b._2,b._1))}}  
  }
}