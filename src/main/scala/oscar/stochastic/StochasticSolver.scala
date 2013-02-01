package oscar.stochastic

import oscar.stochastic._
import scala.util.continuations._

trait DistrSolver[S] {

  def getNextStochasticRealization[B](distr: ContinuousDistr[B]): B
  //def apply[B](distr: Choice[B]): B
  def getNextStochasticRealization[B,T <: S](ch: DiscreteDistr[B]): B @cpsParam[Option[T], Option[T]]
}

trait StochasticSolver[S] extends DistrSolver[S] {
  val random = new scala.util.Random
  override def getNextStochasticRealization[B](distr: ContinuousDistr[B]) = distr.getNextStochasticRealization(random)
  //  override def getNextStochasticRealization[A <: T, B](ch: DiscreteDistr[B]): B @cpsParam[Unit, A] = ch.getNextStochasticRealization()
  override def getNextStochasticRealization[B,T <: S](ch: DiscreteDistr[B]): B @cpsParam[Option[T], Option[T]] = ch.getNextStochasticRealization(random)
}

trait DeterministicSolver[S] extends StochasticSolver[S]{
  override val random = new scala.util.Random(123456789)
}

trait Meanalizable[T <: Meanalizable[T]] {
  implicit def apply(v: T): Meanalizable[T]
  def +(v: T): T
  def *(p: Double): T
}
//
//object DoubleMeanalizable {
//  val zero = 0.0
//  implicit def apply(v: Double)= new DoubleMeanalizable(v)
//}
//
//final class DoubleMeanalizable(d: Double) extends Meanalizable[Double] {
//  override def +(v: Double) = d + v
//  override def *(p: Double) = d * p
//  def apply() = d
//}
//
//trait EsperanceSolver[T <: Meanalizable[T]] extends DistrSolver[T] {
//
//  override def getNextStochasticRealization[B](distr: ContinuousDistr[B]) = distr.mean
//  override def getNextStochasticRealization[B,U : T](ch: DiscreteDistr[B]) = {
//    shift { k: (B => Option[U]) =>
//        def agg(list: Traversable[(Double, B)], acc: Option[U]): Option[U] = {
//          list match {
//            case Nil => acc
//            case (p, v) :: tail =>
//              k(v) match {
//                case None => None
//                case Some(d) =>
//                  acc match {
//                    case None     => agg(tail, Some((d*p)))
//                    case Some(d2) => agg(tail, Some(d2 + (d*p)))
//                  }
//              }
//          }
//        }
//        agg(ch.list, None) 
//    }
//  }
//  //ch.getNextStochasticRealization()
//
//}

