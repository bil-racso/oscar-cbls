package oscar.stochastic

import java.util.TreeMap
import scala.collection._
import collection.JavaConversions._
import scala.collection.JavaConverters._

trait Operator[B] extends Ordering[B] {
  def zero: B
  def compare(a: B, b: B): Int
}
trait Operationable[B] extends Any {
  def +(a: B): B
  def -(a: B): B
  def *(a: B): B
  def *#(b: Double): B
  def /#(b: Double): B
  def /(a: B): B
  def >(a: B): Boolean
  def <(a: B): Boolean
  def =+-(b:B)(implicit op: Operator[B]): Boolean
}
trait RootSquarable[B] extends Any with Operationable[B] {
  def sqrt: B
}

class IntOp(val n: Int) extends AnyVal with Operationable[Int] {
  def +(a: Int) = n + a
  def -(a: Int) = n - a
  def *(a: Int) = n * a
  def *#(b: Double) = (n * b).toInt
  def /#(b: Double) = (n.toDouble / b).toInt
  def /(a: Int) = n / a
  def >(a: Int) = n > a
  def <(a: Int) = n < a
  def =+-(b:Int)(implicit op: Operator[Int])= n==b
}
object IntOp extends Operator[Int] {
  def zero = 0
  def compare(a: Int, b: Int) = a - b
}
class DoubleOp(val n: Double) extends AnyVal with RootSquarable[Double] {
  def +(a: Double) = n + a
  def -(a: Double) = n - a
  def *(a: Double) = n * a
  def *#(b: Double) = n * b
  def /#(b: Double) = n / b
  def /(a: Double) = n / a
  def >(a: Double) = n > a
  def <(a: Double) = n < a
  def sqrt = scala.math.sqrt(n)
  def =+-(b:Double)(implicit op: Operator[Double])= op.compare(n,b) == 0
}
object DoubleOp extends Operator[Double] {
  def zero = 0
  def compare(a: Double, b: Double) =
    if (a < b) +1
    else if (a > b) +1
    else 0
}
class LongOp(val n: Long) extends AnyVal with Operationable[Long] {
  def +(a: Long) = n + a
  def -(a: Long) = n - a
  def *(a: Long) = n * a
  def *#(b: Double) = (n * b).toInt
  def /#(b: Double) = (n.toDouble / b).toInt
  def /(a: Long) = n / a
  def >(a: Long) = n > a
  def <(a: Long) = n < a
  def =+-(b:Long)(implicit op: Operator[Long])= n==b
}
object LongOp extends Operator[Long] {
  def zero = 0l
  def compare(a: Long, b: Long) =
    if (a < b) +1
    else if (a > b) +1
    else 0
}

trait TableView[B] extends Function1[Int, B] with Iterable[(Int, B)] with Traversable[(Int, B)] {
  def map(f: B => B) = TableView(this, f) //treemap.view.map(_ match { case ((t, v)) => (t, f(v)) })
  def map(f: (Int, B) => (Int, B)) = TableView(this, f)
  override def equals(other: Any): Boolean = {
    if (other.isInstanceOf[TableView[B]]) {
      val that = other.asInstanceOf[TableView[B]]
      if (that.iterator.size != this.iterator.size) false
      else {
        val iter1 = that.iterator
        val iter2 = this.iterator
        while (iter1 hasNext) {
          if (iter1.next != iter2.next) return false
        }
        true
      }

    } else false
  }
  //override def filter(f: ((Int, B)) => Boolean) = this.filter(f)
  //override def foreach[U](f: ((Int, B)) => U) = iterator.foreach(f)
}

object TableView {
  def apply[B](table: TableView[B], f: B => B) = new ApplyView1[B](table, (t, v) => (t, f(v)))
  def apply[B](table: TableView[B], f: (Int, B) => (Int, B)) = new ApplyView1[B](table, f)

}

class TableFunction[B <% Operationable[B]](implicit op: Operator[B]) extends TableView[B] {

  val treemapjava = new TreeMap[Int, B]
  val treemap = treemapjava.asScala.withDefaultValue(op.zero)

  def domain = treemap.keySet
  def apply(t: Int) = treemap(t)
  def update(t: Int, v: B) = treemap.put(t, v)
  def isInDomain(t: Int) = treemap.contains(t)
  //def view2(f2: TableFunction[B], f: (B,B) => B) = treemap.view.map( _ match{case ((t,v)) => (t,f(v,f2(t)))})

  def +(f2: TableFunction[B]) = new ApplyView2(this, f2, (a: B, b: B) => a + b)
  def max(f2: TableFunction[B]) = new ApplyView2(this, f2, (a: B, b: B) => if (a > b) a else b)
  def *(m: B) = map { m * _ }

  def add[C <: TableView[B]](f2: C) {
    for (((t, v)) <- f2) {
      this(t) += v
    }
  }
  def delay(d: Int) = map((t: Int, v: B) => (t + d, v)) //for( (t,v) <- this.treemap.view ) yield (t+d,v)
  override def equals(that: Any) = {
    if (that.isInstanceOf[TableFunction[B]]) {
      val f2 = that.asInstanceOf[TableFunction[B]]
      f2.treemap.equals(treemap)
    } else super.equals(that)
  }
  override def toString = treemap.toString()
  def iterator = treemap.iterator
}

//
//class ApplyIterator1[B](g: TableView[B], f: B => B) extends Iterator[(Int, B)] {
//  val iter = g.iterator
//  def hasNext = iter.hasNext
//  def next = {
//    val en = iter.next
//    (en._1, f(en._2))
//  }
//}

class ApplyIterator2[B](g: TableFunction[B], h: TableFunction[B], f: (B, B) => B)(implicit op: Operator[B]) extends Iterator[(Int, B)] {
  val iterG = g.treemap.entrySet().iterator()
  val iterH = h.treemap.entrySet().iterator()

  var vnextG: Option[java.util.Map.Entry[Int, B]] = if (iterG.hasNext()) Some(iterG.next()) else None
  var vnextH: Option[java.util.Map.Entry[Int, B]] = if (iterH.hasNext()) Some(iterH.next()) else None

  def nextG: Option[java.util.Map.Entry[Int, B]] = {
    val res = vnextG
    vnextG = if (iterG.hasNext()) Some(iterG.next()) else None
    res
  }
  def nextH: Option[java.util.Map.Entry[Int, B]] = {
    val res = vnextH
    vnextH = if (iterH.hasNext()) Some(iterH.next()) else None
    res
  }

  def hasNext = vnextG != None || vnextH != None

  def next = {
    (vnextG, vnextH) match {
      case (None, Some(a)) => { nextH; (a.getKey(), f(a.getValue(), op.zero)) }
      case (Some(a), None) => { nextG; (a.getKey(), f(op.zero, a.getValue())) }
      case (Some(a), Some(b)) => {
        if (a.getKey() < b.getKey()) {
          nextG; (a.getKey(), f(a.getValue(), op.zero))
        } else if (a.getKey() == b.getKey()) {
          nextG
          nextH
          (a.getKey(), f(a.getValue(), b.getValue()))
        } else {
          nextH; (b.getKey(), f(op.zero, b.getValue()))
        }
      }
      case q => throw new RuntimeException(q.toString())
    }
  }
}

class ApplyView1[B](g: TableView[B], f: (Int, B) => (Int, B)) extends TableView[B] {
  def apply(i: Int) = f(i, g(i))._2
  def iterator = g.iterator.map { (a) => f(a._1, a._2) }
  //new ApplyIterator1[B](g, f)
  override def toString() = {
    var res = "{"
    for ((k, v) <- iterator) res += k + "=" + v + ","
    res += "}"
    res
  }

}

class ApplyView2[B](g: TableFunction[B], h: TableFunction[B], f: (B, B) => B)(implicit op: Operator[B]) extends TableView[B] {
  def apply(i: Int) = f(g(i), h(i))
  def iterator = new ApplyIterator2(g, h, f)
}

object TableFunction {

  def apply[B <% Operationable[B]](fb: Iterable[(Int, B)])(implicit op: Operator[B]): TableFunction[B] = {
    val f = new TableFunction[B]
    for ((k, v) <- fb.iterator) {
      f(k) = v
    }
    f
  }
  def sum[B <% Operationable[B]](list: Traversable[TableView[B]])(implicit op: Operator[B]) = {
    val res = new TableFunction[B]()
    for (f <- list) res add f
    res
  }
  import scala.collection._

  implicit def fb2tf[B <: Operationable[B]](fb: Iterable[(Int, B)])(implicit op: Operator[B]) = apply(fb)

  def main(args: Array[String]) {

    val f = new TableFunction[Int]

    f(2) = 4

    println(f);

    for (((k: Int, v: Int)) <- f) {
      println("f(" + k + ") = " + v)
    }

    val sum = TableFunction[Int](f + f)

    println(sum)
    println(TableFunction(sum max f) * 3)

  }
}