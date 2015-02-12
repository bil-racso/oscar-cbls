/**
 * *****************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 * ****************************************************************************
 */
/**
 * *****************************************************************************
 * Contributors:
 *     This code has been initially developed by Yoann Guyot.
 * ****************************************************************************
 */

package oscar.cbls.test.search.combinators

import org.scalatest.FunSuite
import oscar.cbls.search.core.ConstantMoveNeighborhood
import oscar.cbls.search.core.NoMoveNeighborhood
import oscar.cbls.search.move.Move
import oscar.cbls.search.core.MoveFound
import oscar.cbls.search.core.NoMoveFound

class CombinatorsTestSuite extends FunSuite {
  import CombinatorGenerator._

  val VERBOSE = false
/*
  test("'a orElse b' returns a's move when a finds a move.") {
    val a = ConstantMoveNeighborhood(oneObjMove)
    val result = (a orElse NoMoveNeighborhood).getMove()
    assert(result === MoveFound(oneObjMove))
  }

  // FIXME: the test should check that b is not even called at all
  test("'a orElse b' returns a's move when a finds a move (even if b move is better).") {
    val a = ConstantMoveNeighborhood(oneObjMove)
    val b = ConstantMoveNeighborhood(zeroObjMove)
    val result = (a orElse b).getMove()
    assert(result === MoveFound(oneObjMove))
  }

  test("'a orElse b' returns b's move when a finds no move but b does.") {
    val b = ConstantMoveNeighborhood(zeroObjMove)
    val result = (NoMoveNeighborhood orElse b).getMove()
    assert(result === MoveFound(zeroObjMove))
  }

  test("'a orElse b' returns no move when neither a nor b find a move.") {
    val result = (NoMoveNeighborhood orElse NoMoveNeighborhood).getMove()
    assert(result === NoMoveFound)
  }

  test("'a best b' returns a's move if it is better than b's move.") {
    val a = ConstantMoveNeighborhood(zeroObjMove)
    val b = ConstantMoveNeighborhood(oneObjMove)
    val result = (a best b).getMove()
    assert(result === MoveFound(zeroObjMove))
  }

  test("'a best b' returns a's move if b finds no move.") {
    val a = ConstantMoveNeighborhood(zeroObjMove)
    val result = (a best NoMoveNeighborhood).getMove()
    assert(result === MoveFound(zeroObjMove))
  }

  test("'a best b' returns b's move if it is better than a's move.") {
    val a = ConstantMoveNeighborhood(oneObjMove)
    val b = ConstantMoveNeighborhood(zeroObjMove)
    val result = (a best b).getMove()
    assert(result === MoveFound(zeroObjMove))
  }

  test("'a best b' returns b's move if a finds no move.") {
    val b = ConstantMoveNeighborhood(zeroObjMove)
    val result = (NoMoveNeighborhood best b).getMove()
    assert(result === MoveFound(zeroObjMove))
  }

  test("'a best b' returns no move when neither a nor b find a move.") {
    val result = (NoMoveNeighborhood best NoMoveNeighborhood).getMove()
    assert(result === NoMoveFound)
  }
  */
}

object CombinatorGenerator {
  val zeroObjMove = Move(0, "zeroObjMove") {}
  val oneObjMove = Move(1, "oneObjMove") {}
}
