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
package oscar.cbls.search.combinators

import oscar.cbls.invariants.core.computation.{CBLSIntVar, Solution, Store}
import oscar.cbls.objective.Objective
import oscar.cbls.search.core._
import oscar.cbls.search.move.{CompositeMove, CallBackMove, Move}

import scala.language.implicitConversions

//TODO: les combinateurs devraient avoir une liste de voisinnages (ou neighborhood*), pas juste un seul.
//TODO: restart
//TODO: il faut un moyen pour passer les paramètres (modèle, acceptor, etc.) de manière standard.

/**
 * @author renaud.delandtsheer@cetic.be
 */
abstract class NeighborhoodCombinator(a:Neighborhood*) extends Neighborhood{
  //this resets the internal state of the move combinators
  override def reset(){
    for(n <- a) n.reset()
  }

  override def verbose_=(i: Int): Unit = {
    for(n <- a) n.verbose = i
    super.verbose_=(i)
  }

  override def toString: String = this.getClass.getSimpleName + "(" + a.mkString(",") + ")"
}

class ProtectBest(a:Neighborhood, i:CBLSIntVar) extends NeighborhoodCombinator(a){

  var oldObj = i.value
  val s:Store = i.model
  var best:Solution = s.solution()
  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean): SearchResult = {
    if(i.value < oldObj){
      best = s.solution(true)
      oldObj = i.value
    }
    a.getImprovingMove(acceptanceCriteria)
  }

  def restoreBest(){
    if (best != null && i.value > oldObj){
      s.restoreSolution(best)
      if(verbose >= 1) println("restoring best solution (obj:" + oldObj + ")")
    }else if(verbose >= 1) println("no better solution to restore")
  }

  /**same as doAllImprovingMoves and calling restoreBest after.
   * @param shouldStop a function that takes the iteration number and returns true if search should be stopped
   *                   eg if the problem is considered as solved
   *                   you can evaluate some objective function there such as a violation degree
   * @param acceptanceCriterion a criterion for accepting a move
   *                            by default, we on
   * @return the number of moves performed
   */
  def doAllImprovingMovesAndRestoreBest(shouldStop:Int => Boolean, acceptanceCriterion:(Int,Int) => Boolean = (oldObj,newObj) => oldObj > newObj):Int = {
    val toReturn = doAllImprovingMoves(shouldStop, acceptanceCriterion)
    restoreBest()
    toReturn
  }

  }

/** this combinator attaches a custom code to a given neighborhood.
  * the code is called whenever a move is asked to the neighborhood.
  * @param a a neighborhood
  * @param proc the procedure to execute before the neighborhood is queried
  */
class DoOnQuery(a:Neighborhood, proc: () =>Unit) extends NeighborhoodCombinator(a){
  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean): SearchResult = {
    proc()
    a.getImprovingMove(acceptanceCriteria)
  }
}

/** this combinator attaches a custom code to a given neighborhood.
  * the code is called whenever a move from this neighborhood is taken
  * The callBack is performed before the move is actually taken.
  * @param a a neighborhood
  * @param proc the procedure to execute when the move is taken
  * @param procOnMove a procedure that inputs the move that is applied;
  *                   use this to update a Tabu for instance
 * @param procAfterMove a procedure to execute after the move is taken
 * @param procAfterMoveOnMove a procedure to execute after the move is taken, with the move as input parameter
 */
class DoOnMove(a:Neighborhood, proc: ()=>Unit, procOnMove:Move => Unit = null, procAfterMove:()=>Unit = null, procAfterMoveOnMove:Move=>Unit = null) extends NeighborhoodCombinator(a){
  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean): SearchResult = {
    a.getImprovingMove(acceptanceCriteria) match {
     case m:MoveFound =>
       CallBackMove(m.m, callBackBeforeMove(m.m), callBackAfterMove(m.m))
     case x => x
    }
  }

  def callBackBeforeMove(m:Move)(){
    if (proc != null) proc()
    if (procOnMove!= null) procOnMove(m)
  }

  def callBackAfterMove(m:Move)(){
    if (procAfterMove != null) procAfterMove()
    if (procAfterMoveOnMove!= null) procAfterMoveOnMove(m)
  }
}

/** this combinator attaches a custom code to a given neighborhood.
  * the code is called whenever a move from this neighborhood is taken for the first time.
  * notice that this neighborhood is reset, so first time can occur several times.
  * @param a a neighborhood
  * @param proc the procedure to call on one first move that is performed from this neighborhood
  */
class DoOnFirstMove(a:Neighborhood, proc: ()=>Unit) extends NeighborhoodCombinator(a){
  var isFirstMove = true
  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean): SearchResult = {
    if (isFirstMove) {
      a.getImprovingMove(acceptanceCriteria) match {
        case m: MoveFound => CallBackMove(m.m, notifyMoveTaken)
        case x => x
      }
    }else{
      a.getImprovingMove(acceptanceCriteria)
    }
  }

  //this resets the internal state of the move combinators
  override def reset() {
    isFirstMove = true
    super.reset()
  }

  private def notifyMoveTaken(){
    proc()
    isFirstMove = false
  }
}


/** this composer randomly tries one neighborhood.
  * it trie the other if the first did not find any move
  * @param a a neighborhood
  * @param b another neighborhood
  * @author renaud.delandtsheer@cetic.be
  */
class Random(a:Neighborhood, b:Neighborhood) extends NeighborhoodCombinator(a,b){
  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean): SearchResult = {
    var currentIsA:Boolean = math.random > 0.5
    def search(canDoMore:Boolean):SearchResult = {
      val current = if(currentIsA) a else b
      current.getImprovingMove(acceptanceCriteria) match{
        case NoMoveFound => currentIsA = !currentIsA ; if (canDoMore) search(false) else NoMoveFound
        case x:MoveFound => x
      }
    }
    search(true)
  }
}

/** this composer sequentially tries all neighborhoods until one move is found
  * between calls, it will roll back to the first neighborhood
  * it tries a first, and if no move it found, tries b
  * a is reset if it did not find anything.
  * @param a a neighborhood
  * @param b another neighborhood
  * @author renaud.delandtsheer@cetic.be
  */
class OrElse(a:Neighborhood, b:Neighborhood) extends NeighborhoodCombinator(a,b){
  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean): SearchResult = {
    a.getImprovingMove(acceptanceCriteria) match{
      case NoMoveFound => a.reset() ; b.getImprovingMove(acceptanceCriteria)
      case x => x
    }
  }
}

/**this composer always selects the best move between the two parameters
  * notice that this combinator makes more sense
  * if the two neighborhood return their best found move,
  * and not their first found move, as usually done.
  * @author renaud.delandtsheer@cetic.be
  */
class Best(a:Neighborhood, b:Neighborhood) extends NeighborhoodCombinator(a,b){

  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean): SearchResult = {
    (a.getImprovingMove(acceptanceCriteria),b.getImprovingMove(acceptanceCriteria)) match{
      case (NoMoveFound,x) => x
      case (x,NoMoveFound) => x
      case (x:MoveFound,y:MoveFound) => if (x.objAfter < y.objAfter) x else y

    }
  }
}

/**this composer is stateful.
  * it returns the result of the first Neighborhood until it returns NoMoveFound.
  * It then switches to the other Neighborhood.
  * it does not come back to the first one after the second one is exhausted
  * @author renaud.delandtsheer@cetic.be
  */
class Exhaust(a:Neighborhood, b:Neighborhood) extends NeighborhoodCombinator(a,b){
  var currentIsA = true
  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean): SearchResult = {
    def search():SearchResult = {
      val current = if(currentIsA) a else b
      current.getImprovingMove(acceptanceCriteria) match{
        case NoMoveFound => if(currentIsA){currentIsA = false; search()} else NoMoveFound
        case x:MoveFound => x
      }
    }
    search()
  }

  //this resets the internal state of the move combinators
  override def reset(){
    currentIsA = true
    super.reset()
  }
}

/** retries n times the move before concluding to noMove can be found
  * resets o nhe first found move, or on reset
  * @param a the neighborhood on which we will perform retries
  * @param n the maximal number of retries on a before concluding it is dead
  */
class Retry(a:Neighborhood, n:Int = 1) extends NeighborhoodCombinator(a){
  var remainingTries = n
  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean): SearchResult = {
    a.getImprovingMove(acceptanceCriteria) match{
      case NoMoveFound =>
        remainingTries -= 1
        if (remainingTries == 0) NoMoveFound
        else this.getImprovingMove(acceptanceCriteria)
      case x =>
        remainingTries = n
        x
    }
  }

  //this resets the internal state of the move combinators
  override def reset(){
    super.reset()
    remainingTries = n
  }
}

class NoReset(a:Neighborhood) extends NeighborhoodCombinator(a){
  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean) = a.getImprovingMove(acceptanceCriteria)

  //this resets the internal state of the move combinators
  override def reset(){}
}
/**this composer is stateful.
  * it returns the result of one Neighborhood until it returns NoMoveFound.
  * It then switches to the other Neighborhood.
  * it starts with Neighborhood a
  * @author renaud.delandtsheer@cetic.be
  */
class ExhaustBack(a:Neighborhood, b:Neighborhood) extends NeighborhoodCombinator(a,b){
  var currentIsA = true
  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean): SearchResult = {
    def search():SearchResult = {
      val current = if(currentIsA) a else b
      current.getImprovingMove(acceptanceCriteria) match{
        case NoMoveFound =>
          if (currentIsA) {
            currentIsA = false
            b.reset()
            b.getImprovingMove(acceptanceCriteria)
          } else {
            currentIsA = true
            a.reset()
            a.getImprovingMove(acceptanceCriteria)
          }
        case x:MoveFound => x
      }
    }
    search()
  }

  //this resets the internal state of the move combinators
  override def reset(){
    currentIsA = true
    super.reset()
  }
}

/**
 * @author renaud.delandtsheer@cetic.be
 */
class ResetOnExhausted(a:Neighborhood) extends NeighborhoodCombinator(a){
  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean): SearchResult = {
    a.getImprovingMove(acceptanceCriteria)  match{
      case NoMoveFound =>
        a.reset()
        a.getImprovingMove(acceptanceCriteria)
      case m:MoveFound => m
    }
  }
}

/**this composer is stateful.
  * it returns the result of the first Neighborhood until it returns NoMoveFound.
  * It then switches to the other Neighborhood,
  * but only if a move was found by the first neighborhood
  * it does not come back to the first one after the second one is exhausted
  * @author renaud.delandtsheer@cetic.be
  */
class ExhaustAndContinueIfMovesFound(a:Neighborhood, b:Neighborhood) extends NeighborhoodCombinator(a,b){
  var currentIsA = true
  var movesFoundWithCurrent = false
  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean): SearchResult = {
    def search():SearchResult = {
      val current = if(currentIsA) a else b
      current.getImprovingMove(acceptanceCriteria) match{
        case NoMoveFound =>
          if(currentIsA){
            currentIsA = false
            movesFoundWithCurrent = false
            search()
          }
          else NoMoveFound
        case x:MoveFound =>
          movesFoundWithCurrent = true
          x
      }
    }
    search()
  }

  //this resets the internal state of the move combinators
  override def reset(){
    currentIsA = true
    movesFoundWithCurrent = false
    super.reset()
  }
}

/**this composer is stateless, it checks the condition on every invocation. If the condition is false,
  * it does not try the Neighborhood and finds no move.
  * @author renaud.delandtsheer@cetic.be
  */
class Conditional(c:()=>Boolean, b:Neighborhood) extends NeighborhoodCombinator(b){
  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean): SearchResult = {
    if(c()) b.getImprovingMove(acceptanceCriteria)
    else NoMoveFound
  }
}

/**this one bounds the number of time the search is actually performed
  * @author renaud.delandtsheer@cetic.be
  */
class BoundSearches(a:Neighborhood, val maxMove:Int) extends NeighborhoodCombinator(a){
  var remainingMoves = maxMove
  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean): SearchResult = {
    if(remainingMoves >0){
      remainingMoves -= 1
      a.getImprovingMove(acceptanceCriteria)
    }else NoMoveFound
  }

  //this resets the internal state of the move combinators
  override def reset(){
    remainingMoves = maxMove
    super.reset()
  }
}

/**this one bounds the number of moves done with this neighborhood
  * notice that the count is reset by the reset operation
  * @author renaud.delandtsheer@cetic.be
  */
class MaxMoves(a:Neighborhood, val maxMove:Int) extends NeighborhoodCombinator(a){
  var remainingMoves = maxMove
  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean): SearchResult = {
    if (remainingMoves > 0) {
      a.getImprovingMove(acceptanceCriteria) match{
        case m:MoveFound => CallBackMove(m.m,notifyMoveTaken)
        case x => x
      }
    } else NoMoveFound
  }

  //this resets the internal state of the move combinators
  override def reset(){
    remainingMoves = maxMove
    super.reset()
  }

  def notifyMoveTaken(){
    remainingMoves -= 1
  }

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

    * @param b given that the move returned by the first neighborhood is committed, we explore the globally improving moves of this one
    *
    */
  def andThen(b:Neighborhood) = new AndThen(a, b, maxMove)

  /**this will modify the effect of the maxMoves by transforming it into a [[MaxMovesWithoutImprovement]]
    * the initial maxMoves is deleted by this method, and the integer bound is passed to [[MaxMovesWithoutImprovement]]
    */
  def withoutImprovementOver(obj:Objective) = new MaxMovesWithoutImprovement(a, maxMove, obj)
}

/**makes a round robin on the neighborhood. it swaps as soon as one does not find a move
  * and swaps neighborhood after "step" invocations
  * @author renaud.delandtsheer@cetic.be
  */
class RoundRobin(a:Neighborhood, b:Neighborhood, steps:Int = 1) extends NeighborhoodCombinator(a,b){
  var currentStep:Int = steps
  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean): SearchResult = {
    if(currentStep >0){
      currentStep -= 1
      if(currentStep == 0) currentStep = -steps
      a.getImprovingMove(acceptanceCriteria) match{
        case NoMoveFound =>
          currentStep = - steps
          b.reset()
          b.getImprovingMove(acceptanceCriteria)
        case x:MoveFound => x
      }
    }else{
      currentStep += 1
      if(currentStep == 0) currentStep = steps
      b.getImprovingMove(acceptanceCriteria) match{
        case NoMoveFound =>
          currentStep = steps
          a.reset()
          a.getImprovingMove(acceptanceCriteria)
        case x:MoveFound => x
      }
    }
  }

  //this resets the internal state of the move combinators
  override def reset(){
    currentStep = steps
    super.reset()
  }
}

class RoundRobinNoParam(val a:Neighborhood,val b:Neighborhood){
  def step(s:Int):Neighborhood = new RoundRobin(a,b,s)
}

object RoundRobinNoParam{
  implicit def toNeighBorHood(rr:RoundRobinNoParam):Neighborhood = {
    val toReturn = new RoundRobin(rr.a,rr.b,1)
    toReturn.verbose = rr.a.verbose
    toReturn
  }
}

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
  * @param a the first neighborhood, all moves delivered by this one will be considered
  * @param b given that the move returned by the first neighborhood is committed, we explore the globally improving moves of this one
  * @param maxFirstStep the maximal number of moves to consider to the first neighborhood
  * @param maximalIntermediaryDegradation the maximal degradation that is admitted for the intermediary step; the higher, the more moves will be considered
  *
  * @author renaud.delandtsheer@cetic.be
 */
class AndThen(a:Neighborhood, b:Neighborhood, maxFirstStep:Int = 10, maximalIntermediaryDegradation:Int = Int.MaxValue) extends NeighborhoodCombinator(a,b){

  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean): SearchResult = {

    a.reset()

    var remainingFirstSteps = maxFirstStep

    var oldObj:Int = 0
    def instrumentedIntermediaryAcceptanceCriteria(stolenOldObj:Int,intermediaryObj:Int):Boolean = {
      oldObj = stolenOldObj;
      intermediaryObj - stolenOldObj <=  maximalIntermediaryDegradation
    }

    while(remainingFirstSteps > 0){
      remainingFirstSteps -= 1
      a.getImprovingMove(instrumentedIntermediaryAcceptanceCriteria) match{
        case NoMoveFound => return NoMoveFound
        case MoveFound(firstMove) => {
          val touchedVars = firstMove.touchedVariables
          val model = touchedVars.head.model
          val snapshot = model.saveValues(touchedVars:_*)
          firstMove.commit()
          if(amIVerbose) println("AndThen: trying first move " + firstMove)
          def globalAcceptanceCriteria:(Int,Int) => Boolean = (_,newObj) => acceptanceCriteria(oldObj,newObj)
          b.getImprovingMove(globalAcceptanceCriteria) match{
            case NoMoveFound => model.restoreSnapshot(snapshot)
            case MoveFound(secondMove) =>{
              model.restoreSnapshot(snapshot)
              return CompositeMove(List(firstMove,secondMove),
                secondMove.objAfter,
                firstMove.neighborhoodName + "_AndThen_" + secondMove.neighborhoodName)
            }
          }
        }
      }
    }
   NoMoveFound
  }
}


/**bounds the number of tolerated moves without improvements over the best value
  * the count is reset by the reset action.
  * @author renaud.delandtsheer@cetic.be
  */
class MaxMovesWithoutImprovement(a:Neighborhood, val maxMovesWithoutImprovement:Int, obj:Objective) extends NeighborhoodCombinator(a){

  var stepsSinceLastImprovement = 0
  var bestObj = Int.MaxValue

    override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean): SearchResult = {
      if (stepsSinceLastImprovement <= maxMovesWithoutImprovement) {
        a.getImprovingMove(acceptanceCriteria) match{
          case m:MoveFound => CallBackMove(m.m,callBack = null, afterMove = notifyMoveTaken)
          case x => x
        }
      } else{
        if (verbose >= 1) println("MaxStepsWithoutImprovement: reached " + maxMovesWithoutImprovement + " moves without improvement")
        NoMoveFound
      }
    }

    //this resets the internal state of the move combinators
    override def reset(){
      stepsSinceLastImprovement = 0
      super.reset()
    }

    def notifyMoveTaken(){
      val newObj = obj.value
      if(newObj < bestObj){
        bestObj = newObj
        stepsSinceLastImprovement = 0
      }else{
        stepsSinceLastImprovement += 1
      }
    }
}

