/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      Hakan Kjellerstrand (hakank@gmail.com)
 ******************************************************************************/
package oscar.example.cp.hakank

import oscar.cp.modeling._
import oscar.cp.search._

/**
 *
 * SEND+MORE MONEY problem in Oscar.
 * 
 * 
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object SendMoreMoney extends CPModel {

   def main(args: Array[String]) {

      val cp = CPSolver()

      // variables
      val S = CPVarInt(cp, 0 to 9)
      val E = CPVarInt(cp, 0 to 9)
      val N = CPVarInt(cp, 0 to 9)
      val D = CPVarInt(cp, 0 to 9)
      val M = CPVarInt(cp, 0 to 9)
      val O = CPVarInt(cp, 0 to 9)
      val R = CPVarInt(cp, 0 to 9)
      val Y = CPVarInt(cp, 0 to 9)

      cp.solveAll() subjectTo {

        // constraints
        cp.add(       S*1000 + E*100 + N*10 + D +
                      M*1000 + O*100 + R*10 + E ==
            M*10000 + O*1000 + N*100 + E*10 + Y)
        cp.add(S > 0)
        cp.add(M > 0)
	cp.add(alldifferent(Array(S,E,N,D,M,O,R,Y)), Strong)

      } exploration {

         cp.binaryFirstFail(S,E,N,D,M,O,R,Y)
         println((S,E,N,D,M,O,R,Y))

      }
	  
      cp.printStats()

  }

}
