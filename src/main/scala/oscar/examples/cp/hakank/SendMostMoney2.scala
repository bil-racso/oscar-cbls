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
 * SEND+MOST=MONEY problem in Oscar.
 *
 * The objective is to maximize MONEY and
 * print all solutions for that value.
 * 
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object SendMostMoney2 extends CPModel {

   def main(args: Array[String]) {
     var money = send_most_money(0)
     println("\nGot maximum value of MONEY: " + money)
     println("Now we check for all solutions...")
     money = send_most_money(money)
     
  }

   def send_most_money(money: Int) : Int = {
      val cp = CPSolver()

      // variables
      val S = CPVarInt(cp, 0 to 9)
      val E = CPVarInt(cp, 0 to 9)
      val N = CPVarInt(cp, 0 to 9)
      val D = CPVarInt(cp, 0 to 9)
      val M = CPVarInt(cp, 0 to 9)
      val O = CPVarInt(cp, 0 to 9)
      val T = CPVarInt(cp, 0 to 9)
      val Y = CPVarInt(cp, 0 to 9)

      val Money = M*10000 + O*1000 + N*100 + E*10 + Y
      var this_money = money

      // constraints
      // if (money > 0) cp.solveAll() else cp.maximize(Money) subjectTo {
      (if (money > 0) cp.solveAll() else cp.maximize(Money)) subjectTo {

          println("MONEY1: " + money)

          cp.add(       S*1000 + E*100 + N*10 + D +
                        M*1000 + O*100 + S*10 + T ==
              M*10000 + O*1000 + N*100 + E*10 + Y)
          cp.add(S > 0)
          cp.add(M > 0)
    	  cp.add(alldifferent(Array(S,E,N,D,M,O,T,Y)), Strong)

          if (money > 0) {
            cp.add(Money == money)
          }


         } exploration {

           cp.binaryFirstFail(S,E,N,D,M,O,T,Y)
           println((S,E,N,D,M,O,T,Y))
           println("Money: " + Money)
           this_money = Money.getValue()

        }
     
        cp.printStats()

        return this_money

   }

}
