/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

package oscar.examples.des



import oscar.des.engine._
import oscar.stochastic._
import scala.util.continuations._


/**
 * Two machines can be broken, they are two repair person to fix it so it can be done in parallel
 * @author Pierre Schaus, Sebastien Mouthuy
 */
class Machine1(m : Model, name: String) extends Process[Unit](name)(m) {
	
	val liveDur = UniformDiscrete(1, 9)
	val breakDur = UniformDiscrete(3, 4)
	
	def beAlive(): Unit @ suspendable = {
		println(name+" is alive");
		waitDuring (liveDur(m))
		beBroken()
		
	}
	
	def beBroken(): Unit @ suspendable =  {
		println(name+" is broken");
		waitDuring(breakDur(m))
		beAlive()
	}
	
	override def start() = beAlive()
	
	
}

object Machine1 {
	def main(args: Array[String]){
  		val mod = new Model()
  		mod.setTime("5/1/2012 5:00pm")
		val m1 = new Machine1(mod,"machine1")
		val m2 = new Machine1(mod,"machine2")
		mod.simulate(100,true);
	}
}
