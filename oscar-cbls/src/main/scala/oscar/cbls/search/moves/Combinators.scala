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

import oscar.cbls.invariants.core.computation.{Solution, Store, CBLSIntVar}
import language.implicitConversions

//TODO: les combinateurs devraient avoir une liste de voisinnages (ou neighborhood*), pas juste un seul.
//TODO: ajouter la gestion de meilleure solution, jump, restart, et acceptor
//TODO: ajouter un moyen pour instancier les voisinages lors de la construction des combinateurs. il faut un moyen pour passer les paramètres (modèle, acceptor, etc.) de manière standard.
//TODO: tabu


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
  override def getImprovingMove(): SearchResult = {
    if(i.value < oldObj){
      best = s.solution(true)
      oldObj = i.value
    }
    a.getImprovingMove()
  }

  def restoreBest(){
    if (best != null && i.value > oldObj){
      s.restoreSolution(best)
      if(verbose >= 1) println("restoring best solution")
    }else if(verbose >= 1) println("no better solution to restore")
  }
}

/** this combinator attaches a custom code to a given neighborhood.
  * the code is called whenever a move is asked to the neighborhood.
  * @param a
  * @param proc
  */
class DoOnQuery(a:Neighborhood, proc: () =>Unit) extends NeighborhoodCombinator(a){
  override def getImprovingMove(): SearchResult = {
    proc()
    a.getImprovingMove()
  }
}

/** this combinator attaches a custom code to a given neighborhood.
  * the code is called whenever a move from this neighborhood is taken
  * @param a
  * @param proc
  */
class DoOnMove(a:Neighborhood, proc: ()=>Unit) extends NeighborhoodCombinator(a){
  override def getImprovingMove(): SearchResult = {
    a.getImprovingMove() match {
     case m:Move => CallBackMove(m,proc)
     case x => x
    }
  }
}

/** this combinator attaches a custom code to a given neighborhood.
  * the code is called whenever a move from this neighborhood is taken
  * @param a
  * @param proc
  */
class DoOnFirstMove(a:Neighborhood, proc: ()=>Unit) extends NeighborhoodCombinator(a){
  var isFirstMove = true
  override def getImprovingMove(): SearchResult = {
    if (isFirstMove) {
      a.getImprovingMove() match {
        case m: Move => CallBackMove(m, notifyMoveTaken)
        case x => x
      }
    }else{
      a.getImprovingMove()
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
  * @param a
  * @param b
  * @author renaud.delandtsheer@cetic.be
  */
class Random(a:Neighborhood, b:Neighborhood) extends NeighborhoodCombinator(a,b){
  override def getImprovingMove(): SearchResult = {
    var currentIsA:Boolean = math.random > 0.5
    def search(canDoMore:Boolean):SearchResult = {
      val current = if(currentIsA) a else b
      current.getImprovingMove() match{
        case NoMoveFound => currentIsA = !currentIsA ; if (canDoMore) search(false) else NoMoveFound
        case x:Move => x
        case ProblemSolved => ProblemSolved
      }
    }
    search(true)
  }
}

/** this composer sequentially tries all neighborhoods until one move is found
  * between calls, it will roll back to the first neighborhood
  * it tries a first, and if no move it found, tries b
  * a is reset if it did not find anything.
  * @param a
  * @param b
  * @author renaud.delandtsheer@cetic.be
  */
class OrElse(a:Neighborhood, b:Neighborhood) extends NeighborhoodCombinator(a,b){
  override def getImprovingMove(): SearchResult = {
    a.getImprovingMove() match{
      case NoMoveFound => a.reset() ; b.getImprovingMove()
      case ProblemSolved => ProblemSolved
      case x => x
    }
  }
}

/**this composer always selects the best move between the two parameters
  * @author renaud.delandtsheer@cetic.be
  */
class Best(a:Neighborhood, b:Neighborhood) extends NeighborhoodCombinator(a,b){

  override def getImprovingMove(): SearchResult = {
    (a.getImprovingMove(),b.getImprovingMove()) match{
      case (ProblemSolved,x) => ProblemSolved //TODO: avoid calling the b.GetImprobingMove
      case (x,ProblemSolved) => ProblemSolved
      case (NoMoveFound,x) => x
      case (x,NoMoveFound) => x
      case (x:Move,y:Move) => if (x.objAfter < y.objAfter) x else y

    }
  }
}

/**this composer is stateful.
  * it returns the result of the first Neighborhood until it returns NoMoveFound. It then switches to the other Neighborhood.
  * it does not come back to the first one after the second one is exhausted
  * @author renaud.delandtsheer@cetic.be
  */
class Exhaust(a:Neighborhood, b:Neighborhood) extends NeighborhoodCombinator(a,b){
  var currentIsA = true
  override def getImprovingMove(): SearchResult = {
    def search():SearchResult = {
      val current = if(currentIsA) a else b
      current.getImprovingMove() match{
        case NoMoveFound => if(currentIsA){currentIsA = false; search()} else NoMoveFound
        case ProblemSolved => ProblemSolved
        case x:Move => x
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
  * @param a
  * @param n
  */
class Retry(a:Neighborhood, n:Int = 1) extends NeighborhoodCombinator(a){
  var remainingTries = n
  override def getImprovingMove(): SearchResult = {
    a.getImprovingMove() match{
      case NoMoveFound => {
        remainingTries -= 1
        if (remainingTries == 0) NoMoveFound
        else this.getImprovingMove()
      }
      case x => {
        remainingTries = n
        x
      }
    }
  }

  //this resets the internal state of the move combinators
  override def reset(){
    super.reset()
    remainingTries = n
  }
}

class NoReset(a:Neighborhood) extends NeighborhoodCombinator(a){
  override def getImprovingMove() = a.getImprovingMove()

  //this resets the internal state of the move combinators
  override def reset(){}
}
/**this composer is stateful.
  * it returns the result of one Neighborhood until it returns NoMoveFound. It then switches to the other Neighborhood.
  * it starts with Neighborhood a
  * @author renaud.delandtsheer@cetic.be
  */
class ExhaustBack(a:Neighborhood, b:Neighborhood) extends NeighborhoodCombinator(a,b){
  var currentIsA = true
  override def getImprovingMove(): SearchResult = {
    def search():SearchResult = {
      val current = if(currentIsA) a else b
      current.getImprovingMove() match{
        case NoMoveFound => {
          if (currentIsA) {
            currentIsA = false;
            b.reset()
            b.getImprovingMove()
          } else {
            currentIsA = true;
            a.reset()
            a.getImprovingMove()
          }
        }
        case ProblemSolved => ProblemSolved
        case x:Move => x
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
  override def getImprovingMove(): SearchResult = {
    a.getImprovingMove()  match{
      case NoMoveFound => {
        a.reset()
        a.getImprovingMove()
      }
      case m:Move => m
      case ProblemSolved => ProblemSolved
    }
  }
}

/**
 * @author renaud.delandtsheer@cetic.be
 */
class ExhaustAndContinueIfMovesFound(a:Neighborhood, b:Neighborhood) extends NeighborhoodCombinator(a,b){
  var currentIsA = true
  var movesFoundWithCurrent = false
  override def getImprovingMove(): SearchResult = {
    def search():SearchResult = {
      val current = if(currentIsA) a else b
      current.getImprovingMove() match{
        case NoMoveFound =>{
          if(currentIsA){
            currentIsA = false
            movesFoundWithCurrent = false;
            search()
          }
          else NoMoveFound
        }
        case x:Move => {
          movesFoundWithCurrent = true;
          x
        }
        case ProblemSolved => ProblemSolved
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
  override def getImprovingMove(): SearchResult = {
    if(c()) b.getImprovingMove()
    else NoMoveFound
  }
}

/**this one bounds the number of time the search is actually performed
  * @author renaud.delandtsheer@cetic.be
  */
class BoundSearches(a:Neighborhood, val maxMove:Int) extends NeighborhoodCombinator(a){
  var remainingMoves = maxMove
  override def getImprovingMove(): SearchResult = {
    if(remainingMoves >0){
      remainingMoves -= 1
      a.getImprovingMove()
    }else NoMoveFound
  }

  //this resets the internal state of the move combinators
  override def reset(){
    remainingMoves = maxMove
    super.reset()
  }
}

/**this one bounds the number of moves done with this neighborhood
  * @author renaud.delandtsheer@cetic.be
  */
class BoundMoves(a:Neighborhood, val maxMove:Int) extends NeighborhoodCombinator(a){
  var remainingMoves = maxMove
  override def getImprovingMove(): SearchResult = {
    if (remainingMoves > 0) {
      a.getImprovingMove() match{
        case m:Move => CallBackMove(m,notifyMoveTaken)
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
}

/**makes a round robin on the neighborhood. it swaps as soon as one does not find a move
  * and swaps neighborhood after "step" invocations
  * @author renaud.delandtsheer@cetic.be
  */
class RoundRobin(a:Neighborhood, b:Neighborhood, steps:Int = 1) extends NeighborhoodCombinator(a,b){
  var currentStep:Int = steps
  override def getImprovingMove(): SearchResult = {
    if(currentStep >0){
      currentStep -= 1
      if(currentStep == 0) currentStep = -steps
      a.getImprovingMove() match{
        case NoMoveFound =>{
          currentStep = - steps
          b.reset()
          b.getImprovingMove()
        }
        case x:Move => x
        case ProblemSolved => ProblemSolved
      }
    }else{
      currentStep += 1
      if(currentStep == 0) currentStep = steps
      b.getImprovingMove() match{
        case NoMoveFound => {
          currentStep = steps
          a.reset()
          a.getImprovingMove()
        }
        case x:Move => x
        case ProblemSolved => ProblemSolved
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

