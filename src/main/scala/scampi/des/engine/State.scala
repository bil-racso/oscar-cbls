package scampi.des.engine

import scala.util.continuations._
import scampi.invariants._

class BasicState{}
class State[A] extends BasicState {
  

  val atEntry = new Event[A]
  val atLeaving = new Event[A]
  
  def code(param: A): Unit @suspendable={println()}
  
  def run(param:A): Unit@suspendable = {
    atEntry emit param
    code(param)
    atLeaving.emit(param)
  }
}