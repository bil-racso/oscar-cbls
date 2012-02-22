

package scampi.des.engine

import scala.util.continuations._
import scampi.invariants._

class Tank(m: Model, capacity: Double) {

  val load = new Var[Double](0.0)  

  def get(qty: Double): Unit @suspendable = {
    waitFor( load.filter(_ >= qty) )
    load := load - qty
  }

  def put(qty: Double): Unit @suspendable = {

    waitFor(load.filter( _ <= capacity-qty) )
    load := load + qty
  }
}