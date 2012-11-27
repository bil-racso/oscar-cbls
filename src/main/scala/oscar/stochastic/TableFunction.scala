package oscar.stochastic

import java.util.TreeMap
import scala.collection._
import collection.JavaConversions._
import scala.collection.JavaConverters._

trait Operationable[B] extends Ordering[B] with java.util.Comparator[B] {
  implicit def intOp: Operationable[Int] = IntOp
  def +(a: B, b: B): B
  def *(a: B, b: B): B
  def *#(a: B, b: Double): Double
  def **(a: B, b: Double): B
  def /#(a: B, b: Double) = *#(a, 1 / b)
  def /(a: B, b: B): B
  //def *(d: Double, b: B):Double
  def >(a: B, b: B): Boolean
  def <(a: B, b: B): Boolean
  def zero: B
  def positive(v: B) = compare(v, zero) >= 0
  def negative(v: B) = compare(v, zero) <= 0
  override def compare(a: B, b: B) = {
    if (>(a, b)) 1
    else if (<(a, b)) -1
    else 0
  }
}

object IntOp extends Operationable[Int] {
  override def +(a: Int, b: Int) = a + b
  override def *(a: Int, b: Int) = a * b
  override def *#(a: Int, b: Double) = a.toDouble * b
  def **(a: Int, b: Double) = (a*b).toInt
  override def /(a: Int, b: Int) = a / b
  //override def *(d: Double, b: Int) = b*d
  override def >(a: Int, b: Int) = a > b
  override def <(a: Int, b: Int) = a < b
  override def zero = 0
}

object LongOp extends Operationable[Long] {
  override def +(a: Long, b: Long) = a + b
  override def *(a: Long, b: Long) = a * b
  override def *#(a: Long, b: Double) = a.toDouble * b
  override def **(a: Long, b: Double) = (a * b).toLong
  override def /(a: Long, b: Long) = a / b
  //override def *(d: Double, b: Long) = b*d
  override def >(a: Long, b: Long) = a > b
  override def <(a: Long, b: Long) = a < b
  override def zero = 0
}

object DoubleOp extends Operationable[Double] {
  override def +(a: Double, b: Double) = a + b
  override def *(a: Double, b: Double) = a * b
  override def *#(a: Double, b: Double) = a * b
  override def **(a: Double, b: Double) = a * b
  override def /(a: Double, b: Double) = a / b
  //override def *(d: Double, b: Double) = b*d
  override def >(a: Double, b: Double) = a > b
  override def <(a: Double, b: Double) = a < b
  override def zero = 0
}

trait TableView[B] extends Function1[Int, B] with Iterable[(Int, B)] with Traversable[(Int, B)] {
  def map(f: B => B) = TableView(this, f) //treemap.view.map(_ match { case ((t, v)) => (t, f(v)) })
  def map(f: (Int, B) => (Int, B)) = TableView(this, f)
  //override def filter(f: ((Int, B)) => Boolean) = this.filter(f)
  //override def foreach[U](f: ((Int, B)) => U) = iterator.foreach(f)
}

object TableView {
  def apply[B](table: TableView[B], f: B => B) = new ApplyView1[B](table, (t, v) => (t, f(v)))
  def apply[B](table: TableView[B], f: (Int, B) => (Int, B)) = new ApplyView1[B](table, f)

}

class TableFunction[B](implicit val op: Operationable[B]) extends TableView[B] {

  val treemapjava = new TreeMap[Int, B]
  val treemap = treemapjava.asScala.withDefaultValue(op.zero)

  def domain = treemap.keySet
  def apply(t: Int) = treemap(t)
  def update(t: Int, v: B) = treemap.put(t, v)

  //def view2(f2: TableFunction[B], f: (B,B) => B) = treemap.view.map( _ match{case ((t,v)) => (t,f(v,f2(t)))})

  def +(f2: TableFunction[B]) = new ApplyView2(this, f2, op.+)
  def max(f2: TableFunction[B]) = new ApplyView2(this, f2, (a: B, b: B) => if (op.>(a, b)) a else b)
  def *(m: B) = map { b: B => op.*(b, m) }

  def add[C <: TableView[B]](f2: C) {
    for (((t, v)) <- f2) {
      this(t) = op + (this(t), v)
    }
  }
  def delay(d: Int) = map((t: Int, v: B) => (t + d, v)) //for( (t,v) <- this.treemap.view ) yield (t+d,v)
  override def equals(that: Any) = {
    if (that.isInstanceOf[TableFunction[B]]) {
      val f2 = that.asInstanceOf[TableFunction[B]]
      f2.treemap.equals(treemap)
    } else if (that.isInstanceOf[TableView[B]]) super.equals(that)
    else false

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

class ApplyIterator2[B](g: TableFunction[B], h: TableFunction[B], f: (B, B) => B) extends Iterator[(Int, B)] {
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
      case (None, Some(a)) => { nextH; (a.getKey(), f(a.getValue(), h.op.zero)) }
      case (Some(a), None) => { nextG; (a.getKey(), f(g.op.zero, a.getValue())) }
      case (Some(a), Some(b)) => {
        if (a.getKey() < b.getKey()) {
          nextG; (a.getKey(), f(a.getValue(), h.op.zero))
        } else if (a.getKey() == b.getKey()) {
          nextG
          nextH
          (a.getKey(), f(a.getValue(), b.getValue()))
        } else {
          nextH; (b.getKey(), f(g.op.zero, b.getValue()))
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
  override def equals(o: Any) = {
    if (!o.isInstanceOf[TableView[B]]) false
    else {
      val that = o.asInstanceOf[TableView[B]]
      val i1 = this.iterator
      val i2 = that.iterator
      if (i1.size != i2.size) false
      for (tup <- i1) {
        if (tup != i2.next()) false
      }
      true
    }
  }
}

class ApplyView2[B](g: TableFunction[B], h: TableFunction[B], f: (B, B) => B) extends TableView[B] {
  def apply(i: Int) = f(g(i), h(i))
  def iterator = new ApplyIterator2(g, h, f)
}

object TableFunction {
  implicit def intOp: Operationable[Int] = IntOp
  implicit def dOp: Operationable[Double] = DoubleOp
  implicit def lOp: Operationable[Long] = LongOp

  def apply[B](fb: Iterable[(Int, B)])(implicit op: Operationable[B]): TableFunction[B] = {
    val f = new TableFunction[B]
    for ((k, v) <- fb.iterator) {
      f(k) = v
    }
    f
  }
  def sum[B](list: Traversable[TableView[B]])(implicit op: Operationable[B]) = {
    val res = new TableFunction[B]()
    for (f <- list) res add f
    res
  }
  import scala.collection._

  implicit def fb2tf[B](fb: Iterable[(Int, B)])(implicit op: Operationable[B]) = this(fb)

  def main(args: Array[String]) {

      implicit def intOp = IntOp
    val f = new TableFunction[Int]

    f(2) = 4

    println(f);

    for (((k: Int, v: Int)) <- f) {
      println("f(" + k + ") = " + v)
    }

    val sum = TableFunction(f + f)

    println(sum)
    println(TableFunction(sum max f) * 3)

  }
}