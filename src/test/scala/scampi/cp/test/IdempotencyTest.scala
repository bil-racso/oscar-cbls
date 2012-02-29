/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package test.scala.scampi.cp


import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import scampi.cp.core.Constraint
import scampi.cp.core._
import scampi.reversible._
import scampi.cp.core.CPOutcome
import scampi.cp.modeling._

class IdempotencyTest extends FunSuite with ShouldMatchers with CPModel {

   
 
    test("test idempotency") {
      
        var nbCallToPropagate = 0
             
    	class MyCons(val X: CPVarInt,idempotent: Boolean) extends Constraint(X.getStore(), "MyCons") {
 
           if (idempotent) setIdempotent()
           override def setup(l: CPPropagStrength): CPOutcome =  {  
             
            X.callPropagateWhenDomainChanges(this)
	        CPOutcome.Suspend	
           }
  
           override def propagate(): CPOutcome = {
             nbCallToPropagate += 1
             X.removeValue(0)
           }
  
        }
      
        val cp = CPSolver()
        val x = CPVarInt(cp,Set(0,1,2,3))
        cp.add(new MyCons(x,false))
        cp.add(x != 3)
        nbCallToPropagate should equal(2)
        
        
        nbCallToPropagate = 0
        val y = CPVarInt(cp,Set(0,1,2,3))
        cp.add(new MyCons(y,true))
        cp.add(y != 3)
        nbCallToPropagate should equal(1)
        
    	
        
    	
    }
    
    

}

