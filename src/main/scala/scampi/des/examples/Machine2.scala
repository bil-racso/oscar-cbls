/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.des.examples


import scampi.des.engine._
import scala.util.continuations._

/**
 * Two machines can be broken, there is only one repair person that can fix it at a time,
 * so one of the machines must wait if the two machines are broken at the same time
 * @author Pierre Schaus, Sebastien Mouthuy
 */
class Machine2(m : Model, name: String, repairPerson: UnaryResource) extends Process(m,name) {
	
	val liveDur = new scala.util.Random(0)
	val repairDur = new scala.util.Random(0)
	
	
	def alive(): Unit @suspendable = {
		println(name+" is alive")
		m.wait(liveDur.nextInt(10).max(1));
		broken()
	}
	
	def broken(): Unit @ suspendable = {
		println(name+" is broken waiting to be repaired")
		m.request(repairPerson)
		repair()
		
	}
	
	def repair(): Unit @ suspendable ={
		println(name+" being repaired")
		m.wait(repairDur.nextInt(3).max(1));
		m.release(repairPerson)
		alive()
		
	}		
	
	override def start = alive
	
}

object Machine2 {
	def main(args: Array[String]) = {
  		val mod = new Model()
  		val repairPerson = new UnaryResource(mod)
		val m1 = new Machine2(mod,"machine1",repairPerson)
		val m2 = new Machine2(mod,"machine2",repairPerson)
		mod.simulate(100,true)
	}
}