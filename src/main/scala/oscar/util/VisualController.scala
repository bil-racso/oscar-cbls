package oscar.util

import scala.util.continuations._
import scala.collection.SeqLike

object VisualController {
  
  var inPause = false
  var cont: (Unit => Unit) = null

  def withController(block: => Unit @suspendable) = {
    reset {
      pause()
      block
    }
  }
  
  def pause()  = {
    shift {k: (Unit => Unit) =>
      cont = k
      if (!inPause) {
        cont = null
        k()
      }   
  	}
  }
  
  def next() {
    //println("next:"+cont)
    val c = cont
    //cont = null
    if (c != null) c()
  }
  
  def play() {
    inPause = false
    val c = cont
    //cont = null
    if (c != null) c()
  }
  
  implicit def richIterable[A, Repr](xs: SeqLike[A, Repr]) = new {
    def suspendable = new {
      def foreach(yld: A => Unit @suspendable): Unit @suspendable = {
        loop(xs.indices) {
          i => yld(xs(i))
        }
      }
    }
  }

  def loopWhile[T](cond: => Boolean)(body: => (Unit @suspendable)): Unit @suspendable = {
    if (cond) {
      body
      loopWhile[T](cond)(body)
    }
  }

  def loop(r: Range)(body: Int => (Unit @suspendable)): Unit @suspendable = {
    var i = r.start
    loopWhile(i < r.end) {
      val k = i
      body(i)
      i = k + 1
    }
  }  

}