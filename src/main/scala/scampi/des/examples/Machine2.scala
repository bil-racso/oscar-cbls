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
 *  @author pschaus
 */
class Machine2(m : Model, name: String) extends Process(m,name) {
	
	val liveDur = new scala.util.Random(0)
	val repairDur = new scala.util.Random(0)
	val repairPerson = new UnaryResource(m)
	
	def alive(): Unit @suspendable = {
		println(name+" is alive")
		m.wait(liveDur.nextInt(10).max(0));
		broken()
	}
	
	def broken(): Unit @ suspendable = {
		println(name+" is broken waiting to be repaired")
		m.request(repairPerson)
		repair()
		
	}
	
	def repair(): Unit @ suspendable ={
		println(name+" being repaired")
		m.wait(repairDur.nextInt(3).max(0));
		m.release(repairPerson)
		alive()
		
	}		
	
	def firstState = alive
	
}

object Machine2 {
	def main(args: Array[String]) {
  		val mod = new Model()
		val m1 = new Machine2(mod,"machine1")
		m1.run()
		val m2 = new Machine2(mod,"machine2")
		m2.run()
		mod.simulate(100,true)
	}
}