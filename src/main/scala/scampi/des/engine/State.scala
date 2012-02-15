package scampi.des.engine

import scala.util.continuations._
import scala.react._

class BasicState{}

class State[A] extends BasicState {

  val atEntry = new EventSource[A]
  val atLeaving = new EventSource[A]
  
  def code(param: A): Unit @suspendable={println()}
  
  def run(param:A): Unit@suspendable = {
    atEntry emit param
    code(param)
    atLeaving.emit(param)
  }
}