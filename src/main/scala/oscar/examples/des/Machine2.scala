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



import oscar.des.engine
import oscar.stochastic._
import oscar.des.engine._
import oscar.invariants._
import scala.util.continuations._
import akka.util.Duration
	import akka.util.duration._


/**
 * Two machines can be broken, there is only one repair person that can fix it at a time,
 * so one of the machines must wait if the two machines are broken at the same time
 * @author Pierre Schaus, Sebastien Mouthuy
 */
class Machine2(m : Model, name: String, repairPerson: Resource) extends Process(name)(m) {
	
	val liveDur = new UniformDiscrete(1 minutes, 10 minutes)
	val repairDur = new UniformDiscrete(1 minutes, 3 minutes)
	
	
	def alive(): Unit @suspendable = {
		println(name+" is alive")
		waitDuring( liveDur(m));
		broken()
	}
	
	def broken(): Unit @ suspendable = {
		println(name+" is broken waiting to be repaired")
		//m.request(repairPerson)
		repairPerson.request
		repair()
		
	}
	
	def repair(): Unit @ suspendable ={
		println(name+" being repaired")
		m.wait(repairDur(m) );
		//m.release(repairPerson)
		repairPerson.release
		alive()
		
	}		
	
	override def start = alive
	
}

object Machine2 {
	def main(args: Array[String]){
  		val mod = new Model()
  		val repairPerson = Resource.unary(mod)
		val m1 = new Machine2(mod,"machine1",repairPerson)
		val m2 = new Machine2(mod,"machine2",repairPerson)
		mod.simulate(100,true)
	}
}
