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
package oscar.examples.cp.hakank

import oscar.cp.modeling._

import oscar.cp.core._

/**
 *
 * CHOO+CHOO0=TRAIN problem in Oscar.
 * 
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object ChooChooTrain {

   def main(args: Array[String]) {

      val cp = CPSolver()

      // variables
      val C = CPVarInt(cp, 0 to 9)
      val H = CPVarInt(cp, 0 to 9)
      val O = CPVarInt(cp, 0 to 9)
      val T = CPVarInt(cp, 0 to 9)
      val R = CPVarInt(cp, 0 to 9)
      val A = CPVarInt(cp, 0 to 9)
      val I = CPVarInt(cp, 0 to 9)
      val N = CPVarInt(cp, 0 to 9)

      val all = Array(C,H,O,T,R,A,I,N)
      val all_str = Array("C","H","O","T","R","A","I","N")

      cp.solveAll() subjectTo {

        // constraints

        // This give better pre-labeling results:
        // it find C: 5..9
        cp.add(       (C*1000 + H*100 + O*10 + O)*2 == 
               T*10000 + R*1000 + A*100 + I*10 + N)


        // than this, which just find C: 2..9
        // cp.add(          C*1000 + H*100 + O*10 + O 
        //              +   C*1000 + H*100 + O*10 + O == 
        //        T*10000 + R*1000 + A*100 + I*10 + N)

        cp.add(C != 0)
        cp.add(T != 0)
	cp.add(allDifferent(all), Strong)

      } exploration {
        
        println("Before labeling: ")
        for(i <- 0 until all.length) {
          println("char: " + all_str(i) + ": " + all(i))
        }
        println()
  
        cp.binaryFirstFail(all)
          
        println(all.mkString(""))

      }
      
      println()
      cp.printStats()

  }

}
