/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      Hakan Kjellerstrand (hakank@gmail.com)
 ******************************************************************************/
package oscar.examples.cp.hakank

import oscar.cp.modeling._
import oscar.cp.search._

/**
 *
 * Least diff problem in Oscar.
 *
 * Minimize the difference ABCDE - FGHIJ
 * where A..J are distinct digits (0..9).
 * 
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object LeastDiff extends CPModel {

   def main(args: Array[String]) {

      val cp = CPSolver()

      // variables
      val A = CPVarInt(cp, 0 to 9)
      val B = CPVarInt(cp, 0 to 9)
      val C = CPVarInt(cp, 0 to 9)
      val D = CPVarInt(cp, 0 to 9)
      val E = CPVarInt(cp, 0 to 9)
      val F = CPVarInt(cp, 0 to 9)
      val G = CPVarInt(cp, 0 to 9)
      val H = CPVarInt(cp, 0 to 9)
      val I = CPVarInt(cp, 0 to 9)
      val J = CPVarInt(cp, 0 to 9)

      val all = Array(A,B,C,D,E,F,G,H,I,J)
      val X = A*10000+B*1000+C*100+D*10+E
      val Y = F*10000+G*1000+H*100+I*10+J
      val Diff = X - Y

      cp.minimize(Diff) subjectTo {

        // constraints
        cp.add(A > 0)
        cp.add(F > 0)
        cp.add(Diff > 0)
	cp.add(alldifferent(all), Strong)

      } exploration {

        cp.binaryFirstFail(all ++ Array(X,Y,Diff))
         println(Array(A,B,C,D,E).mkString("") + " -" +
                 Array(F,G,H,I,J).mkString("") + " =" +
                 Diff)
      }
	  
      cp.printStats()

  }

}
