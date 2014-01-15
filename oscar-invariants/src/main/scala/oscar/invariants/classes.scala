/**
 *
 * This file describes classes to define and use invariants, that were first
 * introduced in the Comet programming language.
 *
 * Invariants automatically maintain mathematical properties between variables. Each
 * time the value of one of the variables changes, the invariant is responsible
 * to change the value of all other variable (if necessary) so that the property
 * still holds.
 *
 * Please note this is an experimental implementation of invariants. Invariants implemented
 * in the cbls package are more mature.
 *
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

package oscar.invariants
import Numeric.Implicits._
import scala.collection.immutable._
import oscar.invariants._

/**
 * A reactive is an entity that listen to several Occuring. Here, all Occuring
 * it listens to are stored in a list so that this Reactive can be removed and
 * all reactions set to the Occuring are disposed automatically.
 */
trait Reactive {
  val reactingOn = new MyDLL[RDependency[_]]

  /**
   * This methods states that f will be executed each time d throws a notification.
   * This reaction is stored so that when dispose is called on this, the reaction
   * will be disposed too.
   */
  def dependsOn[C](d: Occuring[C])(f: C => Boolean): Dependency[C] = new RDependency[C](for (msg <- d) f(msg))

  class RDependency[A](reaction: Reaction[A]) extends Dependency[A](this, reaction) {
    val elem = reactingOn.add(this)
    def dispose() {
      reaction.dispose()
      elem remove
    }
  }
  def dispose() = {
    for (d <- reactingOn) d.dispose()
    true
  }
}

/**
 * A Dependency represents a triple (Reactive, Occuring, f) stating that the Reactive wants f to be executed
 * each time a notification is thrown from Occuring.
 */
abstract class Dependency[A](val reactive: Reactive, var reaction: Reaction[A]) extends Depending {
  def apply(msg: A) = {
    if (!reaction.apply(msg))
      dispose()
  }

  /**
   * This methods modifies the occuring throwing notifications upon which f is executed.
   */
  def nowReactsOn(d: Occuring[A]) {
    reaction.dispose()
    reaction = d.foreach(reaction.f)
  }
}


/**
 * A Var holds a value with facility to assign a new value and maintain a property through invariants.
 */
object Var {
  def apply[A](value: A) = value match {
    case v:Int => new IncrementalVar[Int](v)
    case v:Double => new IncrementalVar[Double](v)
    case v:Long => new IncrementalVar[Long](v)
  }
}

class Var[A](_value: A) extends Signal[A](_value) {

  /**
   * Assigning a value tho this var.
   */
  def :=(v: A) = emit(v)

  def <=(f: this.type => Any): this.type = {
    f(this)
    this
  } 
}

/** A var with features to allow in/decrements and change a historic */
class IncrementalVar[T:Numeric](v:T) extends Var[T](v) {
  /**
   * Occuring that throws notifications representing the incremental change in the value each time it changes.
   */
  val incChanges = Event[(T, T)]()
  @inline override final def :=(v: T) {
    val old = this()
    super.:=(v)
    incChanges emit (old, v)
  }
  def :+=(v: T) { this := this() + v  }
  def :-=(v: T) { this := this() - v }
}

/**
 * A Var holding a list of values, with facility methods.
 */
class VarList[A]() extends Var[Seq[A]](Nil) {
  val isIncreased = Event[A]()
  val isDecreased = Event[A]()
  
  def add(elem: A) {
    this := this() :+ elem
    isIncreased emit (elem)
  }
  def remove(elem: A) {
    this := this().drop(this().indexOf(elem))
    isDecreased emit (elem)
  }
  def mymap[T](f: A => T) ={
    val res = new VarList[T]
    whenever(isIncreased){ v=> res.add(f(v))}
    whenever(isDecreased){ v=> res.remove(f(v))}
    res
  } 
  def removeAll(f:A => Boolean): Unit = this().filter(f).foreach(remove)
}

abstract class StaticInvariant[R] extends Reactive {

}

class ElementArray[A](arr: scala.collection.immutable.IndexedSeq[Var[A]]) {
  @inline def at(y: IncrementalVar[Int]) = { v: Var[A] =>
    new Element1(arr, y, v)
  }
}
//class ElementArray2[A](arr: IndexedSeq[IndexedSeq[Var[A]]]) {
//  def at(y: Var[Int], z: Var[Int]) = {
//    new Element2(arr, y, z)
//  }
//}

/**
 * Maintains v = x(y())
 */
class Element1[A](x: IndexedSeq[Var[A]], y: IncrementalVar[Int], v: Var[A]) extends StaticInvariant[A] {
  v := x(y())()
  def scope() = y +: (for (v <- x) yield v)
  private var dep = dependsOn(x(y())) { (w: A) =>
    v := w
    true
  }
  val a = dependsOn(y) { (w: Int) =>
    v := x(w)()
    dep nowReactsOn (x(w))
    true
  }
}

/**
 * Maintains result = sum_{v \in list} v()
 * list is immutable, so no Var can be added
 */
class SumInvariant[N:Numeric](val result: IncrementalVar[N], list: List[IncrementalVar[N]]) extends StaticInvariant[N] {
  def scope = (for (v <- list) yield v.incChanges).toIndexedSeq
  var a = 0.asInstanceOf[N]
  for (v <- list.iterator) {
    a += v()
    dependsOn(v incChanges) {
      case (o, n) =>
        result :+= n - o
        true
    }
  }
  result := a
}

/**
 * Maintains result = sum_{i \in list} i
 * list is mutable, adding a var in list will modify the value hold by result
 */
class SumInvariantOnList[N:Numeric](val result: IncrementalVar[N], list: VarList[N]) extends StaticInvariant[N] {
  var a = 0.asInstanceOf[N]
  for (v <- list().iterator) {
    a += v
  }
  result := a
  dependsOn(list.isIncreased) { w =>
    result :+= w
    true
  }
  dependsOn(list.isDecreased) { w =>
    result :-= w
    true
  }
}

/**
 * Maintains result = sum_{v \in list} v()
 * list is mutable, so adding a Var in list will modify the value hold by result
 */
class SumInvariantOnListOfVars[N:Numeric](val result: IncrementalVar[N], list: VarList[IncrementalVar[N]]) extends StaticInvariant[N] {
  var a:N = 0.asInstanceOf[N]
  val mmap = new scala.collection.mutable.HashMap[IncrementalVar[N], Dependency[(N, N)]]
  for (v <- list().iterator) {
    mmap.put(v, dependsOn(v.incChanges) {
      case (o, n) =>
        result :+= n - o
        true
    })
    a += v()
  }
  result := a
  dependsOn(list.isIncreased) { v =>
    result :+= v()
    mmap.put(v, dependsOn(v.incChanges) {
      case (o, n) =>
        result :+= n - o
        true
    })
    true
  }
  dependsOn(list.isDecreased) { v =>
    result :-= v()
    mmap.get(v).get.dispose()
    true
  }
}

object invariants {

  def main(args: Array[String]) {

    val y = new IncrementalVar[Int](1)
    val z = new IncrementalVar[Int](1)
    val x = (for (j <- 0 to 10) yield new IncrementalVar[Int](100 + j))

    val s = new IncrementalVar[Int](0) <= sum(z, y)

    val l = new VarList[IncrementalVar[Int]]

    l.add(y)
    l.add(z)

    val sl = new IncrementalVar[Int](0) <= sumVars[Int](l)

    val f = new IncrementalVar[Int](0) <= (x at y) //(y, z) //new Element1(x,y)
    //
    //    when (sl changes){ x => println("sum changed to " + x )
    //      true}
    //        
    //    when(f changes) { w =>
    //      println("f changed to " + w)
    //      true
    //    }

    val r = (y ~> { w: Int => println("y changed " + w) } | z ~> { w: Int => println("z changed") }) ~> {
      _: Int => println("y or z just changed")
    }
    perform(r) until x(6)

    for (w <- y if w < 100) {
      println("y changed to " + w)
      true
    }

    for (msg <- y; if (msg == 5)) {
      println(msg)
      true
    }

    y := 5
    y := 4
    x(3) := 5
    x(4) := 10
    y := 2
    z := 5
    println("-------")
    x(6) := 87
    y := 7
    z := 79
    l.add(x(1))
    l.remove(y)
    y := 5
  }
}
