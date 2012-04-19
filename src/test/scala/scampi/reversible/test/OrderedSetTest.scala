/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.reversible.test


import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scampi.search._
import scampi.reversible._
import scala.collection.JavaConversions._

class OrderedSetTest extends FunSuite with ShouldMatchers {

   
 
    test("test 1") {
    			
    	val myset = new OrderedSet(3,10)
    	myset.toSet() should equal((3 to 10).toSet())
    	
    	myset.removeValue(3)
    	myset.toSet() should equal((4 to 10).toSet())
    	myset.hasValue(3) should equal(false)
    	
    	myset.removeValue(10)
    	myset.toSet() should equal((4 to 9).toSet())
    	myset.hasValue(10) should equal(false)
    	myset.hasValue(9) should equal(true)
    	
    	myset.removeValue(5)
    	myset.toSet should equal((4 to 9).toSet - 5)
    	myset.hasValue(5) should equal(false)
    	
    	myset.removeValue(6)
    	myset.removeValue(7)
    	myset.removeValue(8)
    	myset.removeValue(9)
    	
    	myset.toSet should equal(Set(4))
    	myset.getSize() should equal(1)
    	
    	println("---------------------------")
    	println(myset.toSet)
    	myset.hasValue(4) should equal(true)
    	myset.removeValue(5)
    	myset.removeValue(4)
    	
    	myset.toSet should equal(Set())
    	myset.getSize() should equal(0)
    	myset.hasValue(4) should equal(false)
    	
    	
    }

}

