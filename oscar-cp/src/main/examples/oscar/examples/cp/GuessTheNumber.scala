package oscar.examples.cp

import oscar.cp.modeling._
import oscar.cp.core._

/**
 * I know a 5 digit number having a property that With a 1 after it,
 * it is three times as large as it would be with a one before it.
 * Guess the number ?
 *
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
object GuessTheNumber extends CPModel with App {

  val digits = Array.fill(5)(CPIntVar(0 to 9))

  // with a one after
  val nb1 = digits(0) * 100000 + digits(1) * 10000 + digits(2) * 1000 + digits(3) * 100 + digits(4) * 10 + 1
  // with a one before 
  val nb2 = digits(0) * 10000 + digits(1) * 1000 + digits(2) * 100 + digits(3) * 10 + digits(4) + 100000

  add(nb1 == (nb2 * 3))
  search { binaryStatic(digits) }

  onSolution {
    println("nb1:" + nb1.value + " nb2:" + nb2.value)
  }

  val stats = start()
  println(stats)
}

// simpler model imagined by JF Puget
object GuessTheNumber2 extends CPModel {
  val x = CPIntVar(0 to 100000)
  add(x *10 + 1 == (x + 100000) * 3)
  println(s"=> $x")
}
