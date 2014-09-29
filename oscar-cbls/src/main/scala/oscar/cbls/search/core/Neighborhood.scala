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

package oscar.cbls.search.core

import oscar.cbls.invariants.core.computation.CBLSIntVar
import oscar.cbls.objective.Objective
import oscar.cbls.search.combinators._
import oscar.cbls.search.move.{CallBackMove, Move}

import scala.language.implicitConversions
import scala.language.postfixOps

abstract sealed class SearchResult
case object NoMoveFound extends SearchResult

case class MoveFound(m:Move) extends SearchResult{
  def commit(){m.commit()}
  def objAfter = m.objAfter
  override def toString:String = m.toString
}

object SearchResult {
  implicit def moveToSearchResult(m: Move): MoveFound = MoveFound(m)
}

abstract class JumpNeighborhood extends Neighborhood{

  def doIt()
  def shortDescription():String

  override def getMove(acceptanceCriterion: (Int, Int) => Boolean = (oldObj,newObj) => oldObj > newObj): SearchResult = {
    CallBackMove(() => doIt,valueAfter, this.getClass.getSimpleName, shortDescription)
  }

  /** returns the value after the move
    * called by getMove, by default, this is Int.MaxValue, which is the correct value for a jump
    * in case your jump does not modify the obj function and you want to include this in the move description, override this method
    * @return
    */
  def valueAfter = Int.MaxValue
}

abstract class JumpNeighborhoodParam[T] extends Neighborhood{

  final def doIt(){
    doIt(getParam)
  }

  def doIt(param:T)

  /**if null is returned, the neighborhood returns NoMoveFound*/
  def getParam:T
  def getShortDescription(param:T):String

  override def getMove(acceptanceCriterion: (Int, Int) => Boolean): SearchResult = {
    val param:T = getParam
    if(param == null) NoMoveFound
    else CallBackMove((param:T) => doIt(param), Int.MaxValue, this.getClass.getSimpleName, () => getShortDescription(param),param)
  }
}

/**
 * @author renaud.delandtsheer@cetic.be
 */
abstract class Neighborhood{


  def getMove(acceptanceCriterion:(Int,Int) => Boolean = (oldObj,newObj) => oldObj > newObj):SearchResult

  //this resets the internal state of the Neighborhood
  def reset(){}

  override def toString: String = this.getClass.getSimpleName

  /** verbosity: 0: none
    * 1: combinators
    * 2: combinators + neighborhoods
    */
  var _verbose:Int = 0
  def verbose:Int = _verbose
  def verbose_=(i:Int){
    _verbose = i
  }

  //the number of characters to display in case a verbose approach is deployed.
  var paddingLength:Int = 100

  protected def amIVerbose = verbose >= 2
  /**
   * @return true if a move has been performed, false otherwise
   */
  def doImprovingMove():Boolean = 0 != doAllMoves(_ >= 1)

    /**
   * @param shouldStop a function that takes the iteration number and returns true if search should be stopped
   *                   eg if the problem is considered as solved
   *                   you can evaluate some objective function there such as a violation degree
   * @param acceptanceCriterion a criterion for accepting a move
   *                            by default, we only accept improving moves, but you could change it
   *                            and accept degrading moves as well (eg for tabu search)
   *                            notice that some moves do not consider the acceptance criterion
   *                            because their purpose is to randomize the current solution.
   * @return the number of moves performed
   */
  def doAllMoves(shouldStop:Int => Boolean = _ => false, acceptanceCriterion:(Int,Int) => Boolean = (oldObj,newObj) => oldObj > newObj):Int = {
    var bestObj = Int.MaxValue
    var prevObj = Int.MaxValue
    var toReturn = 0
    var moveCount = 0
    while(!shouldStop(moveCount)){
      getMove(acceptanceCriterion) match {
        case NoMoveFound =>
          if (verbose >= 1) println("no more move found after " + toReturn + " it")
          return toReturn;
        case m: MoveFound =>
          if (verbose >= 1){

            def nStrings(n: Int, s: String): String = if (n <= 0) "" else s + nStrings(n - 1, s)
            def padToLength(s: String, l: Int) = (s + nStrings(l, " ")).substring(0, l)

            if(m.objAfter != Int.MaxValue) {
              val firstPostfix = if (m.objAfter < prevObj) "-"
              else if (m.objAfter == prevObj) "="
              else "+"

              prevObj = m.objAfter

              val secondPostfix = if (m.objAfter < bestObj) {
                bestObj = m.objAfter
                " #"
              } else if (m.objAfter == bestObj) " °"
              else ""

              println(padToLength(m.toString(), paddingLength) + " " + firstPostfix + secondPostfix)
            }else{
              prevObj = m.objAfter
              println(padToLength(m.toString(), paddingLength))
            }
          }

          m.commit()
          true
      }
      toReturn += 1
      moveCount += 1
    }
    if(verbose >= 1)println("stop criteria met after "+ moveCount+" moves")
    toReturn
  }

  /** this composer randomly tries one neighborhood.
    * it tries the other if the first did not find any move
    * @param b another neighborhood
    * @author renaud.delandtsheer@cetic.be
    */
  def random(b:Neighborhood):Neighborhood = new Random(this,b)

  /** this composer sequentially tries all neighborhoods until one move is found
    * between calls, it will roll back to the first neighborhood
    * it tries a first, and if no move it found, tries b
    * a is reset if it did not find anything.
    * @param b another neighborhood
    * @author renaud.delandtsheer@cetic.be
    */
  def orElse(b:Neighborhood):Neighborhood = new OrElse(this,b)

  /** alias for this maxMoves1 exhaust b
    *
    * @param b
    * @return
    */
  def sequence(b:Neighborhood):Neighborhood = this maxMoves 1 exhaust b

  /**this composer always selects the best move between the two parameters
    * notice that this combinator makes more sense
    * if the two neighborhood return their best found move,
    * and not their first found move, as usually done.
    * @author renaud.delandtsheer@cetic.be
    */
  def best(b:Neighborhood):Neighborhood = new Best(this,b)

  /**this composer is stateful.
    * it returns the result of the first Neighborhood until it returns NoMoveFound.
    * It then switches to the other Neighborhood.
    * it does not come back to the first one after the second one is exhausted
    * @author renaud.delandtsheer@cetic.be
    */
  def exhaust(b:Neighborhood):Neighborhood = new Exhaust(this,b)

  /**this composer is stateful.
    * it returns the result of one Neighborhood until it returns NoMoveFound.
    * It then switches to the other Neighborhood.
    * it starts with Neighborhood a
    * @author renaud.delandtsheer@cetic.be
    */
  def exhaustBack(b:Neighborhood):Neighborhood = new ExhaustBack(this,b)

  /**this composer is stateful.
    * it returns the result of the first Neighborhood until it returns NoMoveFound.
    * It then switches to the other Neighborhood,
    * but only if a move was found by the first neighborhood
    * it does not come back to the first one after the second one is exhausted
    * @author renaud.delandtsheer@cetic.be
    */
  def exhaustAndContinueIfMovesFound(b:Neighborhood) = new ExhaustAndContinueIfMovesFound(this, b)

  /**this composer is stateless, it checks the condition on every invocation. If the condition is false,
    * it does not try the Neighborhood and finds no move.
    * @author renaud.delandtsheer@cetic.be
    */
  def when(c:()=>Boolean):Neighborhood = new Conditional(c, this)

  /**this one bounds the number of time the search is actually performed
    * notice that the count is reset by the reset operation
    * @author renaud.delandtsheer@cetic.be
    */
  def maxSearches(maxMove:Int) = new BoundSearches(this, maxMove)

  /**this one bounds the number of moves done with this neighborhood
    * notice that the count is reset by the reset operation
    * @author renaud.delandtsheer@cetic.be
    */
  def maxMoves(maxMove:Int) = new MaxMoves(this, maxMove)


  /** this is an alias for maxMoves 1
    * @return
    */
  def once = new MaxMoves(this, 1)

  /**bounds the number of tolerated moves without improvements over the best value
    * the count is reset by the reset action.
    * @author renaud.delandtsheer@cetic.be
    */
  def maxMovesWithoutImprovement(maxMove:Int, obj:Objective) = new MaxMovesWithoutImprovement(this, null, maxMove, obj)

  /**makes a round robin on the neighborhood. it swaps as soon as one does not find a move
    * and swaps neighborhood after "step" invocations
    * @author renaud.delandtsheer@cetic.be
    */
  def roundRobin(b:Neighborhood):RoundRobinNoParam = new RoundRobinNoParam(this,b)

  /** this combinator attaches a custom code to a given neighborhood.
    * the code is called whenever a move is asked to the neighborhood.
    * @param proc the procedure to execute before the neighborhood is queried
    */
  def onQuery(proc:  => Unit) = new DoOnQuery(this,() => proc)

  /** this combinator attaches a custom code to a given neighborhood.
    * the code is called whenever a move from this neighborhood is taken
    * The callBack is performed before the move is actually taken.
    * @param proc the procedure to execute when the move is taken
    */
  def beforeMove(proc: => Unit) = new DoOnMove(this,procBeforeMove = (_) => proc)

    /** this combinator attaches a custom code to a given neighborhood.
      * the code is called whenever a move from this neighborhood is taken
      * is gets the applied move in input.
      * The callBack is performed before the move is actually taken.
      * @param procOnMove a procedure that inputs the move that is applied;
      *                   use this to update a Tabu for instance
      */
  def beforeMove(procOnMove:Move => Unit) = new DoOnMove(this,procBeforeMove = procOnMove)

  /** this combinator attaches a custom code to a given neighborhood.
    * the code is called whenever a move from this neighborhood is taken
    * The callBack is performed after the move is actually taken.
    * @param proc the procedure to execute when the move is taken
    */
  def afterMove(proc: => Unit) = new DoOnMove(this, procAfterMove = (_) => proc)

  /** this combinator attaches a custom code to a given neighborhood.
    * the code is called whenever a move from this neighborhood is taken
    * is gets the applied move in input.
    * The callBack is performed after the move is actually taken.
    * @param procOnMove a procedure that inputs the move that is applied;
    *                   use this to update a Tabu for instance
    */
  def afterMove(procOnMove:Move => Unit) = new DoOnMove(this,procAfterMove = procOnMove)


  /** this combinator attaches a custom code to a given neighborhood.
    * the code is called whenever a move from this neighborhood is taken for the first time.
    * notice that this neighborhood is reset, so first time can occur several times.
    * @param proc the procedure to call on one first move that is performed from this neighborhood
    */
  def onFirstMove(proc: => Unit) = new  DoOnFirstMove(this,() => proc)

  def protectBest(i:CBLSIntVar) = new ProtectBest(this, i)

  /** retries n times the move before concluding to noMove can be found
    * resets o nhe first found move, or on reset
    * @param n the maximal number of retries on a before concluding it is dead
    */
  def retry(n:Int = 1) = new Retry(this,n)

  /** to prevent resetting the internal state of this neighborhood
    * @return
    */
  def noReset:Neighborhood = new NoReset(this)

  /** defines a name wor this (composite) neighborhood
    * this will be used as prefix for each move returned by this neighborhood (the original name will still exist)
    * use this for debug and documentation purpose only
    * @param name the name that will prefix all returned moves, used for verbosities
    * @return
    */
  def name(name:String) = new Name(this,name)

  /**to build a composite neighborhood.
    * the first neighborhood is used only to provide a round robin exploration on its possible moves
    * you must ensure that this first neighborhood will perform a hotRestart, so that it will enumerate all its moves
    * internally, this neighborhood will be called with a fully acceptant acceptanceCriteria,
    *
    * the move combinator for every move provided by the first neighborhood, the combinator calls the second one
    * and we consider the composition of the two moves for the acceptance criteria.
    * the returned move is the composition of the two found moves
    *
    * you must also ensure that the two neighborhood evaluate the same objective function,
    * since this combinator needs to evaluate the whole composite move, and not only the last part of the composition
    *
    * A native composite neighborhood will probably be much faster than this combinator, so use this for prototyping
    * for instance, this combinator does not allow for some form of symmetry breaking, unless you are really doing it the hard way.
    *
    * this move will reset the first neighborhood on every call, since it is probably bounded by the number of moves it can provide
    *
    * notice that you can use the following better syntax:
    * {{{
    *   myFirstNeighborhood maxMoves 5 andThen mySecondNeighborhood
    * }}}
    *
    * since the proper mechanism is built into Neighborhood.maxMoves; see [[oscar.cbls.search.combinators.MaxMoves]]
    *
    * @param b given that the move returned by the first neighborhood is committed, we explore the globally improving moves of this one
    * @param maxFirstStep the maximal number of moves to consider to the first neighborhood
    *
    * @author renaud.delandtsheer@cetic.be
    */
  def andThen(b:Neighborhood, maxFirstStep:Int) = new AndThen(this, b, maxFirstStep)

  /**
   * tis combinator overrides the acceptance criterion given to the whole neighborhood
   * this can be necessary if you have a neighborhood with some phases only including simulated annealing
   * @param overridingAcceptanceCriterion the acceptance criterion that is used instead of the one given to the overall sear
   */
  def withAcceptanceCriterion(overridingAcceptanceCriterion:(Int,Int) => Boolean)
  = new WithAcceptanceCriterion(this,overridingAcceptanceCriterion)

  /**
   * this combinator overrides accepts all moves (this is the withAcceptanceCriteria, given the fully acceptant criterion
   */
  def acceptAll = new WithAcceptanceCriterion(this,(_:Int,_:Int) => true)

  /**
   * proposes a round-robin with that.
   * notice that you can chain steps; this will build a round-robin on the whole sequence (although this operation is not associative, so better not use parentheses)
   * @param b
   * @return
   */
  def step(b:Neighborhood) = new RoundRobin(List(this,b))


  /**calls the neighborhood until an improvement over obj is achieved
    * the improvement is "since the last reset"
    * @param minMoves the min number of queries that will be forwarded to a (priority over the improvement)
    * @param maxMove the max number of queries that will be forwarded to a (priority over the improvement)
    * @param obj the obj that is looked for improvement
    * @author renaud.delandtsheer@cetic.be
    * */
  def untilImprovement(obj:CBLSIntVar, minMoves:Int = 0, maxMove:Int = Int.MaxValue) = new UntilImprovement(this, obj, minMoves, maxMove)

  /**
   * this combinator injects a simulated annealing acceptation function
   * the criterion is:
   * math.random < math.exp(-gain / temperatureValue)
   * @param temperature a function tht inputs the number of moves taken, and outputs a temperature, for use in the criterion
   */
  def metropolis(temperature:Int => Float = _ => 100) = new Metropolis(this, temperature)
}

/** a neighborhood that never finds any move (quite useless, actually)
  */
case object NoMoveNeighborhood extends Neighborhood{
  override def getMove(acceptanceCriterion:(Int,Int) => Boolean): SearchResult = NoMoveFound
}

/**
 * This neighborhood always returns the same move, given in the constructor
 * @param m the move to return when the neighborhood is queried for a move
 */
case class ConstantMoveNeighborhood(m:Move) extends Neighborhood{
  override def getMove(acceptanceCriterion:(Int,Int) => Boolean): SearchResult = m
}

/**
 * This is an easier way to implement your neighborhood; it provides a simplified interface and hides away searching for the best move vs. the first move
 * and the management of the acceptingCriterion.
 *
 * to implement a neighborhood, you must implement the method searchImprovingMoveEasy
 * in this method, you evaluate moves, and to notify that a move has been explored you have two possibilities:
 *
 * either you do
 * {{{
 * if notifyMoveExplored(newObj, =>myMove) return
 *}}}
 * or you do
 *{{{
 * if(moveRequested(newObj) && submitFoundMove(myMove)) return
 *}}}
 *
 * The second option is more efficient since it will not create a closure on each call
 * and the move will only be instantiated when needed
 *
 * You can also save some state on return, eg if your neighborhood performs some hotRestart:
 *{{{
 * if(moveRequested(newObj) && submitFoundMove(myMove)){
 *   hotRestartForNextTime = ...
 *   return
 * }
 *}}}
 *
 * @param best true if you want the best move false if you want the first acceptable move
 * @param obj the objective function. notice that it is actually a function. if you have an [[oscar.cbls.objective.Objective]] there is an implicit conversion available
 * @param neighborhoodName the name of the neighborhood, used for verbosities
 */
abstract class EasyNeighborhood(best:Boolean = false, obj:()=>Int, neighborhoodName:String=null) extends Neighborhood{

  //passing parameters, and getting return values from the search
  private var oldObj:Int=0
  private var acceptanceCriterion:(Int,Int) => Boolean=null
  private var toReturnMove:Move = null
  private var bestNewObj:Int = Int.MaxValue

  override final def getMove(acceptanceCriterion:(Int,Int) => Boolean):SearchResult = {
    oldObj = obj()
    this.acceptanceCriterion = acceptanceCriterion
    toReturnMove = null
    bestNewObj = Int.MaxValue

    exploreNeighborhood()

    if(toReturnMove == null || (best && !acceptanceCriterion(oldObj,bestNewObj))) {
      if (amIVerbose) println(neighborhoodName + ": no move found")
      NoMoveFound
    }else {
      if (amIVerbose) println(neighborhoodName + ": move found")
      toReturnMove
    }
  }

  /** This is the method you ust implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  def exploreNeighborhood()

  /**
   * @param newObj the new value of the objective function if we perform the move
   * @param m a function that returns the found move. We expect a function here because the move might not need to be instantiated
   * @return true if the search must be stopped right now
   */
  def notifyMoveExplored(newObj:Int, m: =>Move):Boolean = {

    if (best) {
      if (newObj < bestNewObj) {
        bestNewObj = newObj
        toReturnMove = m
      }
    } else if (acceptanceCriterion(oldObj, newObj)) {
      toReturnMove = m
      return true
    }
    false
  }


  /**
   * @param newObj the new value of the objective function
   * @return true if the move is requested, then you should call submitFoundMove
   */
  def moveRequested(newObj:Int):Boolean = {
    if (best) {
      if (newObj < bestNewObj) {
        bestNewObj = newObj
        toReturnMove = null
        return true
      }
    } else if (acceptanceCriterion(oldObj, newObj)) {
      return true
    }
    false
  }

  /** you can only, and must call this method when you called moveRequested and it returned true
    * @param m the move. notice that the obj must be accurate
    * @return true if the search must be stopped right now (you can save some internal state by the way if you need  to, e.g. for a hotRestart
    */
  def submitFoundMove(m:Move):Boolean = {

    if (best) {
      bestNewObj = m.objAfter
      toReturnMove = m
      false
    } else{ //we do not check acceptance criterion here anymore since it was tested in moveRequested, and it could be non-deterministic
      toReturnMove = m
      true
    }
  }
}
