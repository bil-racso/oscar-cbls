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


import oscar.des.engine._


/**
 * Two machines can be broken, they are two repair person to fix it so it can be done in parallel
 * @author pschaus
 */
class Machine1(m : Model, name: String) extends Process(m,name) {
	
	val liveDur = new scala.util.Random(0)
	val breakDur = new scala.util.Random(0)
	
	def beAlive() {
		println(name+" is alive");
		m.wait (liveDur.nextInt(10).max(0).toDouble) {
			beBroken()
		}
	}
	
	def beBroken() {
		println(name+" is broken");
		m.wait(breakDur.nextInt(2).max(0).toDouble) {
			beAlive()
		}
	}
	
	def run() {
		beAlive()
	}
	
}

object Machine1 {
	def main(args: Array[String]) {
  		val mod = new Model()
		val m1 = new Machine1(mod,"machine1")
		m1.run()
		val m2 = new Machine1(mod,"machine2")
		m2.run()
		mod.simulate(100,true);
	}
}
