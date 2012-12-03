package oscar.cp.constraints

import oscar.cp.core.CPVarInt
import oscar.cp.core.Constraint
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPPropagStrength

/**
 * implement a "share value" constraint
 * variables vars shares a value V if all of them or none take the value V
 * @author François Pelsser
 */

class ShareValue(vars : Array[CPVarInt], value : Int) extends Constraint(vars(0).s, "ShareValue") 
{
	override def setup(l: CPPropagStrength): CPOutcome = 
	{
	  for(v <- vars)
	  {
	    v.callValBindWhenBind(this)
	    v.callValRemoveWhenValueIsRemoved(this)
	  }
	  CPOutcome.Suspend
	}
  
	override def valRemove(x:CPVarInt, v:Int) : CPOutcome = {
	  if(v == value)
	  {
	    removeInAllVars
	    CPOutcome.Success
	  } else 
	    CPOutcome.Suspend
	}
	
	override def valBind(x:CPVarInt) : CPOutcome = {
	  if(x.value == value)
	  {
	    setInAllVars
	    CPOutcome.Success
	  } else 
	    CPOutcome.Suspend
	}
	
	def removeInAllVars = for (v <- vars) v.removeValue(value)
	  
	def setInAllVars = for (v <- vars) v.assign(value)


}