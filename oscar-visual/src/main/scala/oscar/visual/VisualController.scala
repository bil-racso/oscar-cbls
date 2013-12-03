/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.visual

import scala.util.continuations._
import scala.collection.SeqLike

object VisualController {
  
  var inPause = true
  var cont: (Unit => Unit) = null

  def withController(block: => Unit @suspendable) = {
    reset {
      //pause()
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
    cont = null
    if (c != null) c()
  }
  
  def play() {
    inPause = false
    val c = cont
    cont = null
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
