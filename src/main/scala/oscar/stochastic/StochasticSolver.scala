package oscar.stochastic

import oscar.stochastic._
import scala.util.continuations._
import oscar.invariants._

trait DistrSolver[S] {

  def getNextStochasticRealization[B](distr: ContinuousDistr[B]): B
  //def apply[B](distr: Choice[B]): B
  def getNextStochasticRealization[B](ch: DiscreteDistr[B]): B @cpsParam[SuspendableResult[S], SuspendableResult[S]]
}

trait StochasticSolver[S] extends DistrSolver[S] {
  val random = new scala.util.Random
  override def getNextStochasticRealization[B](distr: ContinuousDistr[B]) = distr.getNextStochasticRealization(random)
  //  override def getNextStochasticRealization[A <: T, B](ch: DiscreteDistr[B]): B @cpsParam[Unit, A] = ch.getNextStochasticRealization()
  override def getNextStochasticRealization[B](ch: DiscreteDistr[B]): B @cpsParam[SuspendableResult[S], SuspendableResult[S]] = ch.getNextStochasticRealization(random)
}

trait DeterministicSolver[S] extends StochasticSolver[S]{
  override val random = new scala.util.Random(123456789)
}

trait Meanalizable[T] {
//  implicit def apply(v: T): Meanalizable[T]
  def +(v: T): T
  def *(p: Double): T
}

object DoubleMeanalizable {
  val zero = 0.0
  implicit def apply(v: Double)= new DoubleMeanalizable(v)
}

final class DoubleMeanalizable(d: Double) extends Meanalizable[DoubleMeanalizable] {
  override def +(v: DoubleMeanalizable) = d + v()
  override def *(p: Double) = d * p
  def apply() = d
  override def toString() = d.toString
}

trait EsperanceSolver[U <: Meanalizable[U]] extends DistrSolver[U] {

  override def getNextStochasticRealization[B](distr: ContinuousDistr[B]) = distr.mean
  override def getNextStochasticRealization[B](ch: DiscreteDistr[B]) = {
    shift { k: (B => SuspendableResult[U]) =>
        def agg(list: Traversable[(Double, B)], acc: SuspendableResult[U]): SuspendableResult[U] = {
          list match {
            case Nil => acc
            case (p, v) :: tail =>
              k(v) match {
                case Suspend => Suspend
                case EndResult(d) =>
                  acc match {
                    case End     => agg(tail, EndResult((d*p)))
                    case EndResult(d2) => agg(tail, EndResult(d2 + (d*p)))
                  }
              }
          }
        }
        agg(ch.list, End) 
    }
  }
  //ch.getNextStochasticRealization()

}

