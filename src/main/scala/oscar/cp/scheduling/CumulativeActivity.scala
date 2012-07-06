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

package oscar.cp.scheduling

import oscar.cp.core.CPVarInt;
import oscar.cp.core.Store;

class CumulativeActivity(start : CPVarInt, duration : CPVarInt,  end : CPVarInt,  machine : CPVarInt, resource : CPVarInt) extends Activity(start, duration) {

	def mach() = machine
	def getResource() = resource
	
	/**
	 * smallest quantity of resource
	 */
	def getMinResource() = resource.getMin()
	
	/**
	 * largest quantity of resource
	 */
	def getMaxResource() = resource.getMax()
	
	override def toString() = "dur:"+getDur()+ " in ["+getEST()+","+getLCT()+"[ using ["+getMinResource+","+getMinResource+"] on machine(s) "+machine 
}

object CumulativeActivity {
	
	def apply(start : CPVarInt, duration : CPVarInt, machine : CPVarInt, resource : CPVarInt) = {
		
		new CumulativeActivity(start, duration, start.plus(duration), machine , resource)
	}
	
	def apply(start : CPVarInt, duration : CPVarInt, machine : Int, resource : CPVarInt) = {
		
		val m = new CPVarInt(start.getStore(), machine, machine)
		new CumulativeActivity(start, duration, start.plus(duration), m, resource)
	}
	
	def apply(start : CPVarInt, duration : CPVarInt, machine : CPVarInt, resource : Int) = {
		
		val r = new CPVarInt(start.getStore(), resource, resource)
		new CumulativeActivity(start, duration, start.plus(duration), machine, r)
	}
	
	def apply(start : CPVarInt, duration : CPVarInt, machine : Int, resource : Int) = {
		
		val m = new CPVarInt(start.getStore(), machine, machine)
		val r = new CPVarInt(start.getStore(), resource, resource)
		new CumulativeActivity(start, duration, start.plus(duration), m, r)
	}
	
	/*implicit def var2Left(v:CPVarInt) = Left(v)
	implicit def int2Right(i:Int) = Right(i)
		
	def apply(start : Either[CPVarInt,Int], duration : Either[CPVarInt,Int],  end : Either[CPVarInt,Int],  machine : Either[CPVarInt,Int], resource : Either[CPVarInt,Int]) = {
		val cp = Seq(start,duration,end,machine,resource).find(_.isLeft) match {
			case Some(Left(v)) => v.getStore()
			case None => throw new Exception("No CPVarInt provided")
		}
		
		def getVar(in:Either[CPVarInt,Int]) = in match {
			case Left(v) => v
			case Right(i) => new CPVarInt(cp,i to i)
		}
		
		new CumulativeActivity(getVar(start), getVar(duration), getVar(end), getVar(machine),getVar(resource))
	}*/
}