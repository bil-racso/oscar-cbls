/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.examples.des



import oscar.des.engine
import oscar.stochastic._
import oscar.des.engine._
import oscar.invariants._
import scala.util.continuations._
import org.scala_tools.time.Imports._


/**
 * Two machines can be broken, there is only one repair person that can fix it at a time,
 * so one of the machines must wait if the two machines are broken at the same time
 * @author Pierre Schaus, Sebastien Mouthuy
 */
class Machine2(m : Model[Unit], name: String, repairPerson: Resource) extends Process[Unit](name)(m) {
	val a = (1 minutes).toDuration
	val liveDur = UniformDiscrete(1, 10 ).map(_.minutes)
	val repairDur = UniformDiscrete(1, 3).map(_.minutes)
	
	
	def alive(): Unit @susp = {
		println(name+" is alive")
		waitDuring( liveDur(m) );
		broken()
	}
	
	def broken() = {
		println(name+" is broken waiting to be repaired")
		//m.request(repairPerson)
		request(repairPerson)
		repair()
		
	}
	
	def repair() ={
		println(name+" being repaired")
		waitDuring(repairDur(m) );
		//m.release(repairPerson)
		repairPerson.release
		alive()		
	}		
	
	override def start = alive
	
}

object Machine2 {
	def main(args: Array[String]){
  		val mod = new StochasticModel[Unit]()
  		val repairPerson = Resource.unary(mod)
		val m1 = new Machine2(mod,"machine1",repairPerson)
		val m2 = new Machine2(mod,"machine2",repairPerson)
		mod.simulate(mod.clock().plusDays(100),true)
	}
}
