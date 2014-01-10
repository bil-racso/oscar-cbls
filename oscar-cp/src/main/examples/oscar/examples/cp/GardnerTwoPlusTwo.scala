package oscar.examples.cp

import oscar.cp.modeling._
import oscar.cp.core._

/**
 * Martin Gardner Problem:
 *
 * In this addition problem, each letter stands for a different digit.
 *
 *   T W O
 * + T W O
 * --------
 * F O U R
 *
 * If T = 7 and the letter O represents an even number, what is the only possible value for W
 *
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
object GardnerTwoPlusTwo extends CPModel with App {

  val T = CPVarInt(0 to 9)
  val W = CPVarInt(0 to 9)
  val O = CPVarInt(Set(0, 2, 4, 6, 8)) // even number
  val F = CPVarInt(0 to 9)
  val U = CPVarInt(0 to 9)
  val R = CPVarInt(0 to 9)

  add((T * 100 + W * 10 + O) * 2 == F * 1000 + O * 100 + U + 10 + R)
  add(allDifferent(Array(T, W, O, F, U, R)), Strong)

  search {
    binaryFirstFail(Seq(T, W, O, F, U, R))
  }

  onSolution {
    println("T:" + T + " W:" + W + " O:" + O + " F:" + F + " U:" + U + " R:" + R)
  }

  val stats = start()

  println(stats)
}
