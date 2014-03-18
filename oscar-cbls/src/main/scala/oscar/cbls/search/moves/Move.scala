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

import oscar.cbls.invariants.core.computation.CBLSIntVar

/** this composer randomly tries one neighborhood.
  * it trie the other if the first did not find any move
  * @param a
  * @param b
  */
class Random(a:Neighborhood, b:Neighborhood) extends Neighborhood{
  override def getFirstImprovingMove(): Option[Move] = {
    var currentIsA:Boolean = math.random > 0.5
    def search(canDoMore:Boolean):Option[Move] = {
      val current = if(currentIsA) a else b
      current.getFirstImprovingMove() match{
        case None => currentIsA = !currentIsA ; if (canDoMore) search(false) else None
        case Some(x) => Some(x)
      }
    }
    search(true)
  }
}

/** this composer sequentially tries all neighborhoods until one move is found
  * between calls, it will roll back to the first neighborhood
  * it tries a first, and if no move it found, tries b
  * @param a
  * @param b
  */
class AndThen(a:Neighborhood, b:Neighborhood) extends Neighborhood{
  override def getFirstImprovingMove(): Option[Move] = {
    a.getFirstImprovingMove() match{
      case None => b.getFirstImprovingMove()
      case Some(x) => Some(x)
    }
  }
}

/**this composer always selects the best move between the two parameters*/
class Best(a:Neighborhood, b:Neighborhood) extends Neighborhood{
  override def getFirstImprovingMove(): Option[Move] = {
    (a.getFirstImprovingMove(),b.getFirstImprovingMove()) match{
      case (None,x) => x
      case (x,None) => x
      case (Some(x),Some(y)) => if (x.objAfter < y.objAfter) Some(x) else Some(y)
    }
  }
}

/**this composer is stateful.
  * it returns the result of the first Neighborhood until it returns none. It then switches to the other Neighborhood.
  * it does not come back to the first one after the second one is exhausted
  * */
class Exhaust(a:Neighborhood, b:Neighborhood) extends Neighborhood{
  var currentIsA = true
  override def getFirstImprovingMove(): Option[Move] = {
    def search():Option[Move] = {
      val current = if(currentIsA) a else b
      current.getFirstImprovingMove() match{
        case None => if(currentIsA){currentIsA = false; search()} else None
        case Some(x) => Some(x)
      }
    }
    search()
  }
}

/** a neighborhood that never finds any move
  */
class NoMove extends Neighborhood{
  override def getFirstImprovingMove(): Option[Move] = None
}

/**this composer is stateful.
  * it returns the result of one Neighborhood until it returns none. It then switches to the other Neighborhood.
  * it starts with Neighborhood a
  * */
class ExhaustBack(a:Neighborhood, b:Neighborhood) extends Neighborhood{
  var currentIsA = true
  override def getFirstImprovingMove(): Option[Move] = {
    def search(canDoMore:Boolean):Option[Move] = {
      val current = if(currentIsA) a else b
      current.getFirstImprovingMove() match{
        case None => currentIsA = !currentIsA ; if (canDoMore) search(false) else None
        case Some(x) => Some(x)
      }
    }
    search(true)
  }
}

class BoundMove(a:Neighborhood, var maxMove:Int) extends Neighborhood{
  override def getFirstImprovingMove(): Option[Move] = {
    if(maxMove >0){
      maxMove -= 1
      a.getFirstImprovingMove()
    }else None
  }
}

/**this composer is stateless, it checks the condition on every invocation. If the condition is false,
  * it does not try the Neighborhood and finds no move.
  * */
class Conditional(c:()=>Boolean, b:Neighborhood) extends Neighborhood{
  override def getFirstImprovingMove(): Option[Move] = {
    if(c()) b.getFirstImprovingMove()
    else None
  }
}

/**makes a round robin on the neighborhood. it swaps as soon as one does not find a move
  * and swaps neighborhood after "step" invocations
  * */
class RoundRobin(a:Neighborhood, b:Neighborhood, steps:Int = 1) extends Neighborhood{
  var currentStep:Int = steps
  override def getFirstImprovingMove(): Option[Move] = {
    if(currentStep >0){
      currentStep -= 1
      if(currentStep == 0) currentStep = -steps
      a.getFirstImprovingMove() match{
        case None => currentStep = - steps; b.getFirstImprovingMove()
        case Some(x) => Some(x)
      }
    }else{
      currentStep += 1
      if(currentStep == 0) currentStep = steps
      b.getFirstImprovingMove() match{
        case None => currentStep = steps; a.getFirstImprovingMove()
        case Some(x) => Some(x)
      }
    }
  }
}

class RoundRobinNoParam(val a:Neighborhood,val b:Neighborhood){
  def step(s:Int):Neighborhood = new RoundRobin(a,b,s)
}

object RoundRobinNoParam{
  implicit def toNeighBorHood(rr:RoundRobinNoParam):Neighborhood = new RoundRobin(rr.a,rr.b,1)
}

abstract class Neighborhood{
  def getFirstImprovingMove():Option[Move]

  def doFirstImprovingMove():Boolean =
    getFirstImprovingMove() match{
      case None => false
      case Some(n) => n.comit; true
    }

  def exhaust(b:Neighborhood):Neighborhood = new Exhaust(this,b)
  def exhaustBack(b:Neighborhood):Neighborhood = new ExhaustBack(this,b)
  def andThen(b:Neighborhood):Neighborhood = new AndThen(this,b)
  def best(b:Neighborhood):Neighborhood = new Best(this,b)
  def random(b:Neighborhood):Neighborhood = new Random(this,b)
  def when(c:()=>Boolean):Neighborhood = new Conditional(c, this)
  def roundRobin(b:Neighborhood):RoundRobinNoParam = new RoundRobinNoParam(this,b)
}

abstract class Move(val objAfter:Int){
  def comit()
}

case class AssingMove(i:CBLSIntVar,v:Int, override val objAfter:Int) extends Move(objAfter){
  override def comit() {i := v}
}

case class SwapMove(i:CBLSIntVar,j:CBLSIntVar, override val objAfter:Int) extends Move(objAfter){
  override def comit() {i :=: j}
}

