package oscar.stochastic

import oscar.stochastic._
import scala.util.continuations._

trait DistrSolver[+T] {

  def getNextStochasticRealization[B](distr: ContinuousDistr[B]): B
  //def apply[B](distr: Choice[B]): B
  def getNextStochasticRealization[B](ch: DiscreteDistr[B]): B @cpsParam[Option[T],Option[T]]
}

trait StochasticSolver[+T] extends DistrSolver[T] {
  override def getNextStochasticRealization[B](distr: ContinuousDistr[B]) = distr.getNextStochasticRealization()
//  override def getNextStochasticRealization[A <: T, B](ch: DiscreteDistr[B]): B @cpsParam[Unit, A] = ch.getNextStochasticRealization()
  override def getNextStochasticRealization[B](ch: DiscreteDistr[B]): B @cpsParam[Option[T],Option[T]] = ch.getNextStochasticRealization()
}

trait EsperanceSolver extends DistrSolver[Double] {

  override def getNextStochasticRealization[B](distr: ContinuousDistr[B]) = distr.mean
  override def getNextStochasticRealization[B](ch   :   DiscreteDistr[B]) = {    
    
	  shift { k: (B => Option[Double]) => 
	     Some(ch.list.foldLeft( 0.0 ) { (r, b) => r+(b._1*k(b._2).get) }) }
	  
    //ch.getNextStochasticRealization()
	
  }
}