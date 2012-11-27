package oscar.stochastic

import oscar.stochastic._
import scala.util.continuations._

abstract class DistrSolver[T] {

  def getNextStochasticRealization[B](distr: ContinuousDistr[B]): B
  //def apply[B](distr: Choice[B]): B

  def getNextStochasticRealization[B](ch: DiscreteDistr[B]): B @cpsParam[T,T]
}

class StochasticSolver[T] extends DistrSolver[T] {
  override def getNextStochasticRealization[B](distr: ContinuousDistr[B]) = distr.getNextStochasticRealization()
//  override def getNextStochasticRealization[A <: T, B](ch: DiscreteDistr[B]): B @cpsParam[Unit, A] = ch.getNextStochasticRealization()
  override def getNextStochasticRealization[B](ch: DiscreteDistr[B]): B @cpsParam[T, T] = ch.getNextStochasticRealization()
}

//class EsperanceSolver[T] extends DistrSolver[Double] {
//
//  override def getNextStochasticRealization[B](distr: ContinuousDistr[B]) = distr.mean
//  override def getNextStochasticRealization[A <: T, B](ch: DiscreteDistr[B]) = {    
//	  shift { k: (B => Double) => ch.list.foldLeft( 0.0 ) { (r, b) => r+(b._1*k(b._2)) } }
//  }
//}