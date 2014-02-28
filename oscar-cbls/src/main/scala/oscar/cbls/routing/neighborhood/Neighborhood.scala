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
 *     Renaud De Landtsheer
 *     Yoann Guyot
 * ****************************************************************************
 */
package oscar.cbls.routing.neighborhood

import oscar.cbls.routing.model.MoveDescription
import oscar.cbls.routing.model.PositionInRouteAndRouteNr
import oscar.cbls.routing.model.VRP
import oscar.cbls.routing.model.VRPObjective

/**
* @author renaud.delandtsheer@cetic.be
*/
abstract class Move(val objAfter: Int, val vrp: VRP with MoveDescription) {
  def encodeMove()
  def doMove() {
    vrp.cleanRecordedMoves
    encodeMove
    vrp.commit(false)
  }
}

/**
 * @author renaud.delandtsheer@cetic.be
 */
abstract class Neighborhood() {

  /**
   * @param s the search zone, including the VRP that we are examining
   * @param moveAcceptor a function that given the old and new value of the objective function, tell whether the move is considered as an improvement or not.
   * @return
   */
  final def climbAll(s: SearchZone, moveAcceptor: (Int) => (Int) => Boolean = (oldVal) => (newVal) => newVal < oldVal): Int = {
    var toreturn = 0;

    while (doSearch(s, moveAcceptor, false).found && !s.abort()) {
      toreturn += 1
    }
    return toreturn
  }

  /**
   * performs the first discovered move that yields an improvement
   * @param s the search zone, including the VRP that we are examining
   * @param moveAcceptor a function that given the old and new value of the objective function, tell whether the move is considered as an improvement or not.
   * @return true if a move vans discovered, false otherwise
   */
  final def climbFirst(s: SearchZone, moveAcceptor: (Int) => (Int) => Boolean = (oldVal) => (newVal) => newVal < oldVal): Boolean = {
    s.vrp.cleanRecordedMoves()
    doSearch(s, moveAcceptor, false).found
  }

  /**
   * performs the best discovered move that yields an improvement
   * @param s the search zone, including the VRP that we are examining
   * @param moveAcceptor a function that given the old and new value of the objective function, tell whether the move is considered as an improvement or not.
   * @return true if a move vans discovered, false otherwise
   */
  final def climbBest(s: SearchZone, moveAcceptor: (Int) => (Int) => Boolean = (oldVal) => (newVal) => newVal < oldVal): Boolean = {
    bestImprovingMove(s, moveAcceptor) match {
      case Some(m) =>
        m.doMove; true
      case None => false
    }
  }

  /**
   * search and returns the first improving move that yields an improvement, according to moveAcceptor
   * @param s
   * @return
   */
  final def firstImprovingMove(s: SearchZone, moveAcceptor: (Int) => (Int) => Boolean = (oldVal) => (newVal) => newVal < oldVal): Option[Move] = {
    s.vrp.cleanRecordedMoves()
    doSearch(s, moveAcceptor, true) match {
      case MoveFound(move) => Some(move)
      case _ => None
    }
  }

  final def bestImprovingMove(
    s: SearchZone,
    moveAcceptor: (Int) => (Int) => Boolean = (oldVal) => (newVal) => newVal < oldVal): Option[Move] = {

    var bestMove: Option[Move] = None
    var bestObj = Int.MaxValue
    while (true) {
      firstImprovingMove(s, moveAcceptor) match {
        case None => return bestMove
        case Some(move) if (move.objAfter < bestObj) =>
          bestMove = Some(move)
          bestObj = move.objAfter
        case _ => ()
      }
    }
    None
  }

  /**
   * effectue la recherche et s'arrête au premier mouvement trouvé qui améliore
   *
   * @param s the search zone, including the VRP that we are examining
   * @param returnMove true: returns first improving move false: perform first improving move
   * @return
   */
  protected def doSearch(s: SearchZone, moveAcceptor: (Int) => (Int) => Boolean, returnMove: Boolean): SearchResult


  /**
   * this method evaluates the result of moveAcceptor(objectiveFunction) after having comited the encoded move
   * it return the result of this evaluation
   * it restores the state as it was before the move was comited (and records the move again)
   * except if StayIfImprove is set to true. In this case, it does not restore the state if  moveAcceptor(objectiveFunction) return true
   * Besides, it wipes out the move description
   * @param moveAcceptor says if the move is accepted or not
   * @param StayIfAccept
   * @param vrp
   * @return true if this improved, false otherwise, and the objective function after the move
   */
  def checkEncodedMove(
    moveAcceptor: Int => Boolean,
    StayIfAccept: Boolean,
    vrp: VRPObjective with VRPObjective with MoveDescription): (Boolean, Int) = {
    vrp.commit(true)
    val obj = vrp.getObjective
    val accept: Boolean = moveAcceptor(obj)
    if (accept & StayIfAccept) {
      vrp.cleanRecordedMoves()
      (accept, obj)
    } else {
      vrp.undo()
      (accept, obj)
    }
  }
}

/**
 * primaryNodeIterator is a stateful iteration on nodes, it might be re-used,
 * actually so only consume that you really examined
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 */
// format: OFF (to prevent Eclipse from formatting the following lines)
case class SearchZone(relevantNeighbors: (Int => Iterable[Int]),
                      primaryNodeIterator: Iterator[Int],
                      vrp: VRP with VRPObjective with PositionInRouteAndRouteNr
                               with MoveDescription,
                      abort: Unit => Boolean = (_ => false))
// format: ON

abstract class SearchResult {
  def found: Boolean
}
case class MovePerformed() extends SearchResult {
  def found: Boolean = true
}
case class MoveFound(move: Move) extends SearchResult {
  def found: Boolean = true
}
case class NoMoveFound() extends SearchResult {
  def found = false
}
