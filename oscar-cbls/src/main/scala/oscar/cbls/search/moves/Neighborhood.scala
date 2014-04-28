/*******************************************************************************
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
  ******************************************************************************/

package oscar.cbls.search.moves

abstract sealed class SearchResult
case object NoMoveFound extends SearchResult
case object ProblemSolved extends SearchResult

abstract class Move(val objAfter:Int) extends SearchResult{
  def comit()
}

/**
 * @author renaud.delandtsheer@cetic.be
 */
abstract class Neighborhood{
  def getImprovingMove():SearchResult

  //this resets the internal state of the move combinators
  def reset()

  var _verbose:Boolean = false
  def verbose:Boolean = _verbose
  def verbose_=(b:Boolean){
    _verbose = b
  }

  /**
   * @return true if a move has been performed, false otherwise
   */
  def doImprovingMove():Boolean = (0 != doAllImprovingMoves(1))

  /**
   * @return the number of moves performed
   */
  def doAllImprovingMoves(maxMoves:Int = Int.MaxValue):Int = {
    var toReturn = 0;
    var remainingMoves = maxMoves
    while(remainingMoves != 0){
      getImprovingMove() match {
        case ProblemSolved => {
          if (verbose) println("doAllImprovingMoves: problem solved after " + toReturn + " it")
          return toReturn;
        }
        case NoMoveFound => {
          if (verbose) println("doAllImprovingMoves: no move found after " + toReturn + " it")
          return toReturn;
        }
        case m: Move => {
          if (verbose) println("doAllImprovingMoves: " + m)
          m.comit
          true
        }
      }
      toReturn += 1
      remainingMoves -= 1
    }
    if(verbose)println("doAllImprovingMoves STOP criterion: maxMoves ("+ maxMoves+") performed")
    toReturn
  }

  def random(b:Neighborhood):Neighborhood = new Random(this,b)
  def orElse(b:Neighborhood):Neighborhood = new OrElse(this,b)
  def best(b:Neighborhood):Neighborhood = new Best(this,b)
  def exhaust(b:Neighborhood):Neighborhood = new Exhaust(this,b)
  def exhaustBack(b:Neighborhood):Neighborhood = new ExhaustBack(this,b)
  def exhaustAndContinueIfMovesFound(b:Neighborhood) = new ExhaustAndContinueIfMovesFound(this, b)
  def when(c:()=>Boolean):Neighborhood = new Conditional(c, this)
  def maxSearches(maxMove:Int) = new BoundSearches(this, maxMove)
  def maxMoves(maxMove:Int) = new BoundMoves(this, maxMove)
  def roundRobin(b:Neighborhood):RoundRobinNoParam = new RoundRobinNoParam(this,b)
}
