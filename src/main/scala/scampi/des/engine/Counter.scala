/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/

package scampi.des.engine

import scampi.invariants._
//import scala.collection.mutable._
import java.util.PriorityQueue
import scala.util.continuations._

/**
 * @author Sebastien Mouthuy
 */
class PQCounter(m: EventModel, v: Double) extends SourceSignal(m, v){
	
  val pq = new PriorityQueue[WaitEvent]
  def addEvent(ev: WaitEvent) {
    pq.add(ev)
  }
  def inc(d:Double) = this === this() + d
  def removeEvent(ev: WaitEvent) { pq.remove(ev)}
  override def ===(i:Double) = {
    new PQCounterCond(this,i)
  }
  def nonEmpty = pq.size() > 0
  def generate(){
    while (nonEmpty){
      generateNext()
      m.processPendings
    }
  }
  def generateNext(){
    val ev = next
    ev.process
  }
  def next = {
    val res = pq.poll()
    this set res.time    
    res
  }
}

class PQCounterCond(pqc: PQCounter, v: Double) extends Occuring[Double, Unit]{
  def model = pqc.model
  def foreach(f2: Unit => Boolean) = {
    val a = new WaitEvent(v,{_=>f2();()})
    pqc addEvent(a)
    new PQEventBlock(pqc,a)
  }
  def ===(v: Unit) = this
}


class PQEventBlock[A](pqc: PQCounter, ev: WaitEvent) extends ListDepending[Double]({_=>false}){
  def model = pqc.model
  def dispose(){
    pqc.removeEvent(ev)
  }
}

object Counter{
  
  def main(args: Array[String]){
    
    val m = new Model
    
    val x = new VarInt(m,5)
    val y = new VarInt(m,8)
    
    val c = new PQCounter(m,0)
    
    
    reset{    
      val a = waitFor(x.changes)
      println("500")
    }
    
    once ( c === 5 ){
      println("super")
    }
    println("here")
    x := 500
    
    
    c.generate()
  }
}