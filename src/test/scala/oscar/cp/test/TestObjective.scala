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

package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._

import oscar.cp.modeling._


class TestObjective extends FunSuite with ShouldMatchers  {
  
  
  test("Obj1") {   
	  val cp = CPSolver()
	  val x1 = CPVarInt(cp,1 to 3)
	  val x2 = CPVarInt(cp,1 to 3)
	  cp.add(x1+x2 == 4)
	  
	  val obj1 = new CPObjectiveUnitMinimize(x1,"x1")
	  obj1.tightenMode = TightenType.NoTighten
	  val obj2 = new CPObjectiveUnitMinimize(x2,"x2")
	  obj2.tightenMode = TightenType.NoTighten
  
	  var nbsol = 0
	  
	  val obj = new CPObjective(cp,obj1,obj2)
	  cp.optimize(obj) exploration {
	    cp.binary(Array(x1,x2),cp.minVar,_.max)
	    nbsol +=1
	    
	  }
	  nbsol should be(3) 
  }
  
  test("Obj2") {   
	  val cp = CPSolver()
	  val x1 = CPVarInt(cp,2 to 3)
	  val x2 = CPVarInt(cp,1 to 3)
	  
	  val obj1 = new CPObjectiveUnitMinimize(x1,"x1")
	  obj1.tightenMode = TightenType.WeakTighten
	  val obj2 = new CPObjectiveUnitMinimize(x2,"x2")
	  obj2.tightenMode = TightenType.WeakTighten
  
	  var nbsol = 0
	  
	  val obj = new CPObjective(cp,obj1,obj2)
	  cp.optimize(obj) exploration {
	    cp.binary(Array(x1,x2),cp.minVar,_.max)
	    println(x1+" "+x2)
	    nbsol +=1
	    
	  }
	  nbsol should be(4) 
  } 
  
  test("Obj3") {   
	  val cp = CPSolver()
	  val x1 = CPVarInt(cp,2 to 3)
	  val x2 = CPVarInt(cp,1 to 3)
	  
	  val obj1 = new CPObjectiveUnitMinimize(x1,"x1")
	  obj1.tightenMode = TightenType.StrongTighten
	  val obj2 = new CPObjectiveUnitMinimize(x2,"x2")
	  obj2.tightenMode = TightenType.WeakTighten
  
	  var nbsol = 0
	  
	  val obj = new CPObjective(cp,obj1,obj2)
	  cp.optimize(obj) exploration {
	    cp.binary(Array(x1,x2),cp.minVar,_.max)
	    println(x1+" "+x2)
	    nbsol +=1
	    // solutions are (3,3) (2,3)
	    Set((3,3),(2,3)).contains((x1.value,x2.value)) should be(true)
	  }
	  nbsol should be(2) 
  }    
  
  test("Obj4") {   
	  val cp = CPSolver()
	  val x1 = CPVarInt(cp,2 to 3)
	  val x2 = CPVarInt(cp,1 to 3)
	  
	  val obj1 = new CPObjectiveUnitMinimize(x1,"x1")
	  obj1.tightenMode = TightenType.StrongTighten
	  val obj2 = new CPObjectiveUnitMinimize(x2,"x2")
	  obj2.tightenMode = TightenType.StrongTighten
  
	  var nbsol = 0
	  
	  val obj = new CPObjective(cp,obj1,obj2)
	  cp.optimize(obj) exploration {
	    cp.binary(Array(x1,x2),cp.minVar,_.max)
	    println(x1+" "+x2)
	    nbsol +=1
	    // solutions are (3,3) (2,2)
	    Set((3,3),(2,2)).contains((x1.value,x2.value)) should be(true)
	  }
	  nbsol should be(2) 
  }  
    
    
  
 
  


}
