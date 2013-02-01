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
import org.scala_tools.time.Imports._
import org.joda.time.Minutes


/**
 * Two machines can be broken, they are two repair person to fix it so it can be done in parallel
 * @author Pierre Schaus, Sebastien Mouthuy
 */
class Machine1(m : Model[Unit], name: String) extends Process[Unit](name)(m) {
	
  //implicit def period2Long(p: Period) = p.millis.toLong
	val liveDur  = UniformDiscrete(1, 9).map(_.minutes)
	val breakDur = UniformDiscrete(3, 4).map(_.minutes)
	
	def beAlive(): Unit @cpsParam[Option[Unit],Option[Unit]] = {
		println(name+" is alive")
		waitDuring(liveDur(m).toPeriod)
		beBroken()
		
	}
	
	def beBroken(): Unit @cpsParam[Option[Unit],Option[Unit]] = {
		println(name+" is broken");
		waitDuring(breakDur(m).toPeriod)
		beAlive()
	}
	
	override def start() = beAlive()
	
	
}

object Machine1 {
	def main(args: Array[String]){
  		val mod = new StochasticModel[Unit]()
  		mod.setTime( new DateTime(2012,1,5,17,0))
  		val m1 = new Machine1(mod,"machine1")
		val m2 = new Machine1(mod,"machine2")
		mod.simulate(mod.clock().plusDays(100),true);
	}
}
