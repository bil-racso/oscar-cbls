/*******************************************************************************
  * This file is part of OscaR (Scala in OR).
  *
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/gpl-3.0.html
  ******************************************************************************/

/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by Ghilain Florent.
  ******************************************************************************/

package oscar.cbls.routing.neighborhood
import oscar.cbls.search.SearchEngine
import oscar.cbls.routing.model.{ClosestNeighborPoints, PositionInRouteAndRouteNr, ObjectiveFunction, VRP}

/**
 * Performs a composite move of differents neighborhoods.
 * The search complexity depends on the complexity of the neighborhoods used.
 */
object CompositeMove extends SearchEngine{
  /**
   * Statement to use during the composite search procedure.
   */
  var declaration:CompositeDeclaration = null

  /**
   * It attaches a statement to the search procedure.
   * @param c the statement.
   */
  def attachDeclaration(c:CompositeDeclaration) {declaration = c}

  def nbNeighbor:Int = declaration.nbNeighbor
  def actualNeighbor:Int = declaration.actualNeighbor
  def nextNeighbor:Int = {declaration.actualNeighbor =  (actualNeighbor + 1)%nbNeighbor; actualNeighbor}
  def getkLimited:Int = declaration.kLimited


  /**
   * Returns the composite operator, i.e. which decreases the most the objective value
   * of a given VRP problem.
   * @param vrp the given VRP problem.
   * @return the best two-opt-move operator.
   */
  def getMove(vrp:VRP with ObjectiveFunction with PositionInRouteAndRouteNr with ClosestNeighborPoints,
    previousMove:Neighbor = null):CompositeMove ={
    var move:Neighbor = null
    move = nextMove(vrp,actualNeighbor,previousMove)
    if (move==null)
      CompositeMove(move,vrp.ObjectiveVar.value,vrp)
    else
      CompositeMove(move,move.getObjAfter,vrp)
  }

  def nextMove(vrp:VRP with ObjectiveFunction with PositionInRouteAndRouteNr with ClosestNeighborPoints,i:Int
                ,previousMove:Neighbor):Neighbor = {
    i match{
      case 0 => {
        val move = OnePointMove.getFirstImprovingMove(vrp,vrp.getKNearest(getkLimited),previousMove)
        if(move!= null) move else nextMove(vrp,nextNeighbor,move)
      }
      case 1 => {
        val move = Swap.getFirstImprovingMove(vrp,vrp.getKNearest(getkLimited),previousMove)
        if (move==null) nextMove(vrp,nextNeighbor,move) else {declaration.actualNeighbor=0;move}
      }
      case 2 => {
        val move = ThreeOptA.getFirstImprovingMove(vrp,vrp.getKNearest(getkLimited),previousMove)
        if (move==null) nextMove(vrp,nextNeighbor,move) else {declaration.actualNeighbor=0;move}
      }
      case 3 => {
        val move = ThreeOptB.getFirstImprovingMove(vrp,vrp.getKNearest(getkLimited),previousMove)
        if (move==null) nextMove(vrp,nextNeighbor,move) else {declaration.actualNeighbor=0;move}
      }
      case 4 => {
        val move = TwoOpt.getFirstImprovingMove(vrp,vrp.getKNearest(getkLimited),previousMove)
        if (move!=null) {declaration.actualNeighbor=0;move} else move
      }
    }
  }
}

/**
 * The declaration of the composite move of a given VRP problem.
 * It allows us to specify all characteristics we want to impose to this move;
 * like the neighborhoods, their order, their potential restriction, ..
 * @param vrp the given VRP problem.
 */
case class CompositeDeclaration(vrp:VRP){
  val nbNeighbor = 5
  var actualNeighbor = 0
  var kLimited = 20
}


/**
 * Models a composite-move operator of a given VRP problem. *
 * @param move the move selected.
 * @param objAfter the objective value if we performed this composite-move operator.
 * @param vrp the given VRP problem.
 */
case class CompositeMove(move:Neighbor, objAfter:Int, vrp:VRP) extends Neighbor{
  // overriding methods
  def comit {move.getValuesToAssign.foreach(t => t._1 := t._2)}
  def getObjAfter = objAfter
  def startNodeForNextExploration: Int = move.startNodeForNextExploration
  def getValuesToAssign = move.getValuesToAssign

  override def toString():String = move.toString
}