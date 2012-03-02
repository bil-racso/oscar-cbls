package scampi

import scala.collection.mutable._
import scala.util.continuations._

package object invariants {
  def cpsunit: Unit @cps[Unit] = ()
  def cpsfalse: Boolean @cps[Unit] = false
  def cpstrue: Boolean @cps[Unit] = true
  
type tunit = Unit @suspendable

  @inline def when[A](d: Occuring[_,A])(f: A => Boolean) = {
    d.foreach(f)
  }
  
  
  class Event[A](m: EventModel) extends NotifyAllEvent[A]{
    def model = m
  }
  
  class EventOne[A](m: EventModel) extends NotifyOneEvent[A]{
    def model = m
  }
  
  implicit def occ2occTrue[A](occ: Occuring[A,Boolean]) = occ === true
  implicit def v2cpsV[A](a: A): A @suspendable = a
  implicit def bool2cpsbool(b: Boolean) = if (b) cpstrue else cpsfalse
  implicit def bl2f[A,B](block: => B) = {a:A => block}
  //implicit def rd2r[A](rd: ReactionDescription[A]) = rd.post()
  implicit def occuring2desc[A](occ: Occuring[_,A]) = occ ~~> { _:A => }
  
//  @inline def perform[A](rd: ReactionDescription[A]) = 
//    for ( msg <- rd.occuring ){
//      rd.f(msg)
//      true
//    }
  
  @inline def whenever[A](e: Occuring[_,A])(f: A => Unit) = 
    for ( msg <- e ){
      f(msg)
      true
    }
  
  
  @inline def once[A,B](d: Occuring[A,B])(f: B => Unit) = {
    when(d){(x:B) => 
      f(x)
      false
    }
  }
  @inline def waitFor[A](d: Occuring[_,A]) = {
    val res = shift { k: (A => Unit) =>
      val res = once(d) { msg: A =>
        k(msg)
      }
    }
    res
  }
  
  implicit def Signal2Val[A](v: Signal[_,A]) = {v()}
  implicit def array2ElementArray[A](at: scala.collection.immutable.IndexedSeq[Var[A]]) = {
    new ElementArray(at)
  }
  def sum(l: VarInt*) = {(v: VarInt) => new SumInvariant(v, l.toList)}
  def sumOnList(l: VarList[Int])    = {(v: VarInt) => new SumInvariantOnList(v, l)}
  def sumOnListOfVars(l: VarList[VarInt]) = {(v: VarInt) => new SumInvariantOnListOfVars(v, l)}
  
//  implicit def array2ElementArray2[A](at: IndexedSeq[IndexedSeq[Var[A]]]) = {
//    new ElementArray2(at)
//  }
}