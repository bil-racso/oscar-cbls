package scampi.des.engine

import scala.util.continuations._
import scala.react._

class Tank(m: Model, capacity: Double) {

  var load = new Var[Double](0)

  private var pendings: List[(Double, () => Unit)] = Nil

  def get(qty: Double): Unit @suspendable = {
    m.waitFor[Double](load, _ >= qty)
    load.update(load() - qty)

  }

  def put(qty: Double): Unit @suspendable = {

    m.waitFor[Double](load, _ <= capacity)
    load.update(load() + qty)

  }
}