package oscar.cp.scheduling

import oscar.cp.core.CPVarInt;
import oscar.cp.core.Store;

class CumulativeActivity(start : CPVarInt, duration : CPVarInt,  end : CPVarInt,  machine : CPVarInt, resource : CPVarInt) extends ActivityS(start, duration, end) {

	def getMachine() = machine
	def getResource() = start
	
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