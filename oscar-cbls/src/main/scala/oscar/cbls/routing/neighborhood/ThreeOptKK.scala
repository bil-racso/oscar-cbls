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
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer and Florent Ghilain.
 * ****************************************************************************
 */

package oscar.cbls.routing.neighborhood

import oscar.cbls.search.SearchEngineTrait

/**
 * Removes three edges of routes, and rebuilds routes from the segments.
 * Finds 3 candidate points for a 3-opt move, and then
 * chooses on-the-fly between simple 3-opt move and reverse 3-opt move.
 *
 * Info : it also could be saw as the move of a route's segment to another place.
 * The search complexity is O(n*k*log(k)).
 * @author renaud.delandtsheer@cetic.be
 * THIS IS EXPERIMENTAL
 */
object ThreeOptKK extends Neighborhood with SearchEngineTrait {
  val REVERSE = true // this is a constant used for readability

  // PRE-CONDITION: all nodes of search zone must be routed
  override protected def doSearch(s: SearchZone,
                                  moveAcceptor: (Int) => (Int) => Boolean,
                                  returnMove: Boolean): SearchResult = {
    val vrp = s.vrp
    val startObj: Int = vrp.getObjective()

    /**
     * The insertion point is picked from the primaryNodeIterator.
     */
    while (s.primaryNodeIterator.hasNext) {
      val insertionPoint: Int = s.primaryNodeIterator.next()
      assert(vrp.isRouted(insertionPoint),
        "ThreeOpt should be applied to routed nodes only.")

      val otherNodes:List[List[Int]] = s.relevantNeighbors(insertionPoint)
        .filter(vrp.isRouted)
        .groupBy(n => vrp.routeNr(n).value)
        .toList
        .map(routeAndNodes => routeAndNodes._2.toList)

      case class MoveException(m:SearchResult) extends Exception

      try{
        for(nodeList <- otherNodes){
          exploreNodeList(nodeList)
        }
      }catch {
        case x:MoveException => return x.m
      }

      def exploreNodeList(nodeList:List[Int]){
        nodeList match{
          case head :: tail => exploreTail(head,tail) ; exploreNodeList(tail)
        }
      }

      def exploreTail(head:Int, tail:List[Int]){
        tail match{
          case other :: newtail => explore(head,other) ; exploreTail(head,newtail)
        }
      }

      def explore(a:Int,b:Int){
        val (first,second) = if(vrp.positionInRoute(a).value < vrp.positionInRoute(b).value) (a,b) else (b,a)
        if(vrp.isBetween(insertionPoint, first, second)) return
          ThreeOpt.chooseBest3Opt(first, vrp.next(first).value, second, insertionPoint,
          startObj, returnMove, moveAcceptor, vrp) match {
          case NoMoveFound() => ()
          case result:SearchResult => throw new MoveException(result)
        }
      }
    }

    NoMoveFound()
  }
}