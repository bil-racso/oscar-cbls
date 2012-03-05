package scampi

import scala.collection.mutable._
import scala.util.continuations._

package object invariants {
  def cpsunit: Unit @cps[Unit] = ()
  def cpsfalse: Boolean @cps[Unit] = false
  def cpstrue: Boolean @cps[Unit] = true
  
  @inline def when[A](d: Occuring[A])(f: A => Boolean): Reaction[A] = {
    d.foreach(f)
  }
  
  implicit def bl2f[A,B](block: => B) = {a:A => block}
  implicit def rd2r[A](rd: ReactionDescription[A]) = rd.post()
  implicit def occuring2desc[A](occ: Occuring[A]) = occ ~> { _ => }
  @inline def perform[A](rd: ReactionDescription[A]): Reaction[A] = {
    for ( msg <- rd.occuring ){
      rd.f(msg)
      true
    }
  }
  @inline def whenever[A](e: Occuring[A])(f: A => Unit): Reaction[A] = {
    for ( msg <- e ){
      f(msg)
      true
    }
  }
  
  @inline def once[A](d: Occuring[A])(f: A => Unit) = {
    when(d){(x:A) => 
      f(x)
      false
    }
  }
  @inline def waitFor[A](d: Occuring[A]) = {
    shift { k: (A => Unit) =>
      val a = once(d) { msg: A =>
        k(msg)
      }
    }
  }
  
  implicit def Var2Val[A](v: Var[A]) = {v()}
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