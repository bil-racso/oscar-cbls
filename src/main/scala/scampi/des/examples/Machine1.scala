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
 * Two machines can be broken, they are two repair person to fix it so it can be done in parallel
 * @author Pierre Schaus, Sebastien Mouthuy
 */
class Machine1(m : Model, name: String) extends Process(m,name) {
	
	val liveDur = new scala.util.Random(0)
	val breakDur = new scala.util.Random(0)
	
	def beAlive(): Unit @ suspendable = {
		println(name+" is alive");
		m.wait (liveDur.nextInt(10).max(0).toDouble)
		beBroken()
		
	}
	
	def beBroken(): Unit @ suspendable =  {
		println(name+" is broken");
		m.wait(breakDur.nextInt(2).max(0).toDouble)
		beAlive()
	}
	
	override def start(): Unit @ suspendable =  {
		beAlive()
	}
	
}

object Machine1 {
	def main(args: Array[String]) =  {
  		val mod = new Model()
		val m1 = new Machine1(mod,"machine1")
		val m2 = new Machine1(mod,"machine2")
		mod.simulate(100,true);
	}
}