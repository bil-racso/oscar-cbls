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

package oscar.examples.cp

import oscar.cp.modeling._
import oscar.search._
import oscar.reversible._
import oscar.cp.core._
import scala.io.Source

/** 
 * I know a 5 digit number having a property that With a 1 after it, 
 * it is three times as large as it would be with a one before it. 
 * Guess the number ?
 * 
 * @author Pierre Schaus pschaus@gmail.com

 */
object GuessTheNumber extends App {
    val cp = CPSolver()
    
    
    val digits = Array.fill(5)(CPVarInt(cp,0 to 9))
    
    // with a one after
    val nb1 =  digits(0)*100000 + digits(1)*10000 + digits(2)*1000 +  digits(3)*100 + digits(4)*10 + 1
    // with a one before 
    val nb2 =  digits(0)*10000 + digits(1)*1000 +  digits(2)*100 + digits(3)*10 + digits(4) + 100000

    cp.solve subjectTo {
      cp.add(nb1 == (nb2*3))
    } exploration {
      cp.binary(digits)
      println("nb1:"+nb1.value+" nb2:"+nb2.value)
    } run()
    cp.printStats()
    
    
    // ---------------
    
    // simpler model imagined by JF Puget
    
    val cp1 = CPSolver()
    val x = CPVarInt(cp1,0 to 100000)
    cp1.add(x*10+1 == (x+100000)*3)
    println("=>"+x) // no need to search, already assigned     
}
