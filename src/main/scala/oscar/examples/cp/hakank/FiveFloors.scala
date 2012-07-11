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
import oscar.cp.core._


/**

  Five floors problem in Oscar
 
  From Alexey Radul & Gerald Jay Sussman: 
  "The Art of Propagator", page 34
  """
  Baker, Cooper, Fletcher, Miller, and Smith live on the first
  five floors of this apartment house. Baker does not live on the
  fifth floor. Cooper does not live on the first floor. Fletcher
  does not live on either the fifth or the first floor. Miller lives
  on a higher floor than does Cooper. Smith does not live on a
  floor adjacent to Fletcher'. Fletcher does not live on a floor
  adjacent to Cooper's.
  """

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
 */

object FiveFloors extends CPModel {


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    var n = 5
       
    //
    // decision variables
    //
    val x = Array.tabulate(n)(i => CPVarInt(cp, 1 to n))
    val Array(baker, cooper, fletcher, miller, smith) = x


    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {

       cp.add(alldifferent(x), Strong)

       // Baker does not live on the fifth floor.
       cp.add(baker != 5)

       // Cooper does not live on the first floor. 
       cp.add(cooper != 1)

       // Fletcher does not live on either the fifth or the first floor. 
       cp.add(fletcher != 5)
       cp.add(fletcher != 1)

       // Miller lives on a higher floor than does Cooper. 
       cp.add(miller > cooper)

       // Smith does not live on a floor adjacent to Fletcher'. 
       cp.add((smith-fletcher).abs() > 1)

       // Fletcher does not live on a floor adjacent to Cooper's.
       cp.add((fletcher-cooper).abs() > 1)


     } exploration {
       
       // cp.binary(x)
       // cp.binaryFirstFail(x)
       cp.binaryMaxDegree(x)

       println(x.mkString(""))

       numSols += 1
       
     }

     println("\nIt was " + numSols + " solutions.\n")

     cp.printStats()
   }

}
