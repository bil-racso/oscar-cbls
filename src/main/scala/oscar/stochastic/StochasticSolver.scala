package oscar.stochastic

import oscar.stochastic._
import scala.util.continuations._

trait DistrSolver[+T] {

  def getNextStochasticRealization[B](distr: ContinuousDistr[B]): B
  //def apply[B](distr: Choice[B]): B
  def getNextStochasticRealization[B](ch: DiscreteDistr[B]): B @cpsParam[Option[T], Option[T]]
}

trait StochasticSolver[+T] extends DistrSolver[T] {
  override def getNextStochasticRealization[B](distr: ContinuousDistr[B]) = distr.getNextStochasticRealization()
  //  override def getNextStochasticRealization[A <: T, B](ch: DiscreteDistr[B]): B @cpsParam[Unit, A] = ch.getNextStochasticRealization()
  override def getNextStochasticRealization[B](ch: DiscreteDistr[B]): B @cpsParam[Option[T], Option[T]] = ch.getNextStochasticRealization()
}

trait Meanalizable[T <: Meanalizable[T]] {
  def +(v: T): T
  def *(p: Double): T
}

object DoubleMeanalizable {
  val zero = 0.0
}

sealed class DoubleMeanalizable(d: Double) extends Meanalizable[DoubleMeanalizable] {
  override def +(v: DoubleMeanalizable) = new DoubleMeanalizable(d + v())
  override def *(p: Double) = new DoubleMeanalizable(d * p)
  def apply() = d
}

trait EsperanceSolver[T <: Meanalizable[T]] extends DistrSolver[T] {

  override def getNextStochasticRealization[B](distr: ContinuousDistr[B]) = distr.mean
  override def getNextStochasticRealization[B](ch: DiscreteDistr[B]) = {
    shift { k: (B => Option[T]) =>
        def agg(list: Traversable[(Double, B)], acc: Option[T]): Option[T] = {
          list match {
            case Nil => acc
            case (p, v) :: tail =>
              k(v) match {
                case None => None
                case Some(d) =>
                  acc match {
                    case None     => agg(tail, Some(d*p))
                    case Some(d2) => agg(tail, Some(d2 + d*p))
                  }
              }
          }
        }
        agg(ch.list, None) 
    }
  }
  //ch.getNextStochasticRealization()

}

