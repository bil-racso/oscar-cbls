/**
 * *****************************************************************************
 * This file is part of OscaR (Scala in OR).
 *
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 * ****************************************************************************
 */

package oscar

import scala.collection.mutable._
import scala.util.continuations._

import com.typesafe.scalalogging.slf4j._

class SuspendableIterable[T](iter: scala.collection.immutable.Iterable[T]) {
  def foreach[U](f: T => Unit @cpsParam[U, U]): Unit @cpsParam[U, U] = {
    val i = iter.iterator
    while (i.hasNext) {
      f(i.next)
    }
  }
  def suspendable = this
}

package object invariants extends Logging {

  def cpsunit: Unit @cps[Unit] = ()
  def cpsfalse: Boolean @cps[Unit] = false
  def cpstrue: Boolean @cps[Unit] = true

  @inline def when[A](d: Occuring[A])(f: A => Boolean): Reaction[A] = {
    d.foreach(f)
  }

  implicit def iter2susp[T](iter: scala.collection.immutable.Iterable[T]) = new SuspendableIterable(iter)
  implicit def bl2f[A, B](block: => B) = { a: A => block }
  implicit def rd2r[A](rd: ReactionDescription[A]) = rd.post()
  implicit def occuring2desc[A](occ: Occuring[A]) = occ ~> { _ => }

  def and(occurings: scala.collection.Iterable[Occuring[_]]) = {
      def loop(occ: Occuring[_], others: scala.collection.Iterable[Occuring[_]]): Occuring[_] = {
        if (others.isEmpty) occ
        else loop(occ & others.head, others.tail)
      }
    require(occurings nonEmpty)
    loop(occurings.head, occurings.tail)
  }

  @inline def perform[A](rd: ReactionDescription[A]): Reaction[A] = {
    for (msg <- rd.occuring) {
      rd.f(msg)
      true
    }
  }
  @inline def until[A](s: Signal[Option[A]])(block: => Depending) {
    println("Until: " + s())
    if (s() == None) {
      println("entered")
      val d = block
      for (v <- s; if v != None) {
        d.dispose
        false
      }
    }
  }
  @inline def whenever[A](e: Occuring[A])(f: A => Unit): Reaction[A] = {
    for (msg <- e) {
      f(msg)
      true
    }
  }

  @inline def once[A, T](d: Occuring[A])(f: A => T) = {
    //var res: T
    when(d) { (x: A) =>
      f(x)
      false
    }
  }

  @inline def hangUntil[A, T](d: Occuring[A]): A @cpsParam[SuspendableResult[T], SuspendableResult[T]] = {

    if (logger.underlying.isDebugEnabled()) {
      val e = (new Throwable())

      shift { k: (A => SuspendableResult[T]) =>
        try {
          once(d) { msg: A =>
            k(msg)
          }
        } catch {
          case exception: Throwable =>
            println("Executing Reaction from ")
            for (el <- e.getStackTrace()) {
              //println("   at " + el.getClassName() + "->" + el.getMethodName() + "(" + el.getFileName() + ":" + el.getLineNumber() + ")")
              println("    " + el)
            }
            throw exception
        }
        Suspend
      }
    } else {
      shift { k: (A => SuspendableResult[T]) =>
        once(d) { msg: A =>
          k(msg)
        }
        Suspend
      }
    }
  }

  implicit def Var2Val[A](v: Var[A]) = v()
  implicit def array2ElementArray[A](at: scala.collection.immutable.IndexedSeq[Var[A]]) = {
    new ElementArray(at)
  }
  def sum[N:Numeric](l: IncrementalVar[N]*) = (new SumInvariant(createZero,l.toList)).result
  def sum[N:Numeric](l: VarList[N]) = new SumInvariantOnList(createZero, l).result
  
  def createZero[N:Numeric]: IncrementalVar[N] = new IncrementalVar[N](implicitly[Numeric[N]].zero) 
  
  def sumVars[N:Numeric](l: VarList[IncrementalVar[N]],v: IncrementalVar[N]): IncrementalVar[N] = { new SumInvariantOnListOfVars(v, l); v}
  def sumVars[N:Numeric](l: VarList[IncrementalVar[N]]): IncrementalVar[N] = sumVars(l,createZero[N])

  //  implicit def array2ElementArray2[A](at: IndexedSeq[IndexedSeq[Var[A]]]) = {
  //    new ElementArray2(at)
  //  }
}
