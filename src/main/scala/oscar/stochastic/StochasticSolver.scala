package oscar.stochastic

import oscar.stochastic._
import scala.util.continuations._

abstract class DistrSolver {

  def getNextStochasticRealization[B](distr: ContinuousDistr[B]): B
  //def apply[B](distr: Choice[B]): B
  def getNextStochasticRealization[B](ch: DiscreteDistr[B]): B @suspendable
}

class StochasticSolver extends DistrSolver {
  override def getNextStochasticRealization[B](distr: ContinuousDistr[B]) = distr.getNextStochasticRealization()
//  override def getNextStochasticRealization[A <: T, B](ch: DiscreteDistr[B]): B @cpsParam[Unit, A] = ch.getNextStochasticRealization()
  override def getNextStochasticRealization[B](ch: DiscreteDistr[B]): B @suspendable = ch.getNextStochasticRealization()
}

class EsperanceSolver extends DistrSolver {

  override def getNextStochasticRealization[B](distr: ContinuousDistr[B]) = distr.mean
  override def getNextStochasticRealization[B](ch: DiscreteDistr[B]) = {    
//	  val a = shift { k: (B => Double) => ch.list.foldLeft( 0.0 ) { (r, b) => r+(b._1*k(b._2)) } }
    ch.getNextStochasticRealization()
	
  }
}