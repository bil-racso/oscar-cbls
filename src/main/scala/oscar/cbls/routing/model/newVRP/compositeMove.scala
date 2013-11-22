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
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by De Landtsheer Renaud and Ghilain Florent.
  ******************************************************************************/

package oscar.cbls.routing.model.newVRP

trait MoveDescription extends VRP{
  var Recording = true //recording ou comitted

  private var affects:List[(Int,Int)] = List.empty

  protected def addMove(node:Int,value:Int){
    assert(Recording)
    affects = (node,value) :: affects
  }

  protected case class Segment(start:Int,end:Int)

  def cut(beforeStart:Int,end:Int):Segment = {
    assert(!this.isInstanceOf[PositionInRouteAndRouteNr]
           || this.asInstanceOf[PositionInRouteAndRouteNr].isASegment(beforeStart,end))

    addMove(beforeStart,Next(end).value)
    Segment(Next(beforeStart).value,end)
  }

  def segmentFromUnrouted(n:Int):Segment = {
    assert(!isRouted(n))
    Segment(n,n)
  }

  def reverse(s:Segment): Segment = {
    var prev = s.start
    var current:Int = Next(prev).value
    while(current != s.end){
      addMove(current,prev)
      prev = current
      current = Next(current).value
    }
    Segment(s.end,s.start)
  }

  def insert(s:Segment,node:Int){
    addMove(node,s.start)
    addMove(s.end,Next(s.start).value)
  }

  def append(s:Segment,t:Segment):Segment = {
    addMove(s.end,t.start)
    Segment(s.start,t.end)
  }

  def unroute(s:Segment){
    var current = s.start
    unroute(current)
    while(current != s.end){
      current = Next(current).value
      addMove(current,N)
    }
  }

  def commit(withRollBack:Boolean){
    assert(Recording)
    if (withRollBack){
      var undoList:List[(Int,Int)] = List.empty
      def doIt(toDo:List[(Int,Int)]){
        toDo match{
          case head :: tail => {
            doIt(tail)
            undoList = (head._1,Next(head._1).value) :: undoList
            Next(head._1) := head._2
          }
          case Nil => ;
        }
      }
      doIt(affects)
      affects = undoList
      assert({Recording = false; true})
    }else{
      doAllMoves()
      affects = List.empty
    }
  }

  private def doAllMoves(){
    def doIt(toDo:List[(Int,Int)]){
      toDo match{
        case head :: tail => doIt(tail); Next(head._1) := head._2
        case Nil => ;
      }
    }
    doIt(affects)
  }

  def rollBack(){
    assert(!Recording)
    assert({Recording = true ; true})
    doAllMoves()
  }
}

trait MoveDescriptionSmarter extends MoveDescription with Predecessors{
  def cutAt(start:Int,end:Int):Segment = {
    cut(this.preds(start).value,end)
  }

  def cutNode(n:Int):Segment = {
    cut(this.preds(n).value,n)
  }
}

