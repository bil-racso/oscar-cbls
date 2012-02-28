/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package test.scala.scampi.invariants

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.collection.immutable._
import scampi.invariants._

/**
 * Test functionality of Invariants
 * @author Sebatien Mouthuy & Pierre Schaus
 */
class InvariantsTest extends FunSuite with ShouldMatchers  {

  
  test("test whener on VarInt") {
	  	val y = new VarInt(1)
	  	
	  	var nb = 0
	  	
	  	whenever ( y.filter(_ == 5) ){
	  	 	nb += 1
	  	}
	  	
	  	y := 5
	  	nb should equal(1)
	  	y := 6
	  	nb should equal(1)
	  	y := 5
	  	nb should equal(2)  	
  }
  
  test("test once on VarInt") {
	  	val y = new VarInt(1)
	  	
	  	var nb = 0
	  	
	  	once ( y.filter(_ == 5) ){
	  	 	nb += 1
	  	}
	  	
	  	y := 5
	  	nb should equal(1)
	  	y := 6
	  	nb should equal(1)
	  	y := 5
	  	nb should equal(1)  	
  }
  
  
	
 
  

}

