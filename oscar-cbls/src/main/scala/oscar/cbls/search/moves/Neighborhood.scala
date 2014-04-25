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
  def doImprovingMove():Boolean =
    getImprovingMove() match{
      case ProblemSolved => {
          if (verbose) println("doImprovingMove: problem solved")
          false
        }
      case NoMoveFound => {
        if (verbose) println("doImprovingMove: no move found")
        false
      }
      case m:Move => {
        if(verbose) println("doImprovingMove: move: " + m)
        m.comit
        true
      }

    }

  /**
   * @return the number of moves performed
   */
  def doAllImprovingMoves(maxMoves:Int = Int.MaxValue):Int = {
    var toReturn = 0;
    var remainingMoves = maxMoves
    while(remainingMoves != 0 && doImprovingMove()){
      toReturn += 1
      remainingMoves -= 1
    }
    if(verbose){
      println("doAllImprovingMoves completed with " + toReturn + " moves")
      if(remainingMoves == 0) println("doAllImprovingMoves STOP criterion: maxMoves performed")
      else println("doAllImprovingMoves STOP criterion: no more move found")
    }
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



