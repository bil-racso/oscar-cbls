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
 *         by Renaud De Landtsheer
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
        .groupBy(vrp.routeNr(_).value)
        .toList
        .map(_._2.toList)

      for(nodeList <- otherNodes){
        for((a,b) <- makeAllUnsortedPairs(nodeList)){
          val (first,second) = if(vrp.positionInRoute(a).value < vrp.positionInRoute(b).value) (a,b) else (b,a)
          if(!vrp.isBetween(insertionPoint, first, second) && !(vrp.next(insertionPoint).value == first)){
            ThreeOpt.chooseBest3Opt(first, vrp.next(first).value, second, insertionPoint,
              startObj, returnMove, moveAcceptor, vrp) match {
              case m:NoMoveFound => ()
              case result:SearchResult => return result
            }
          }
        }
      }
    }//end while
    NoMoveFound()
  }

  /**
   * @param l a list
   * @return a list of all pairs of element made from the elements in l
   */
  def makeAllUnsortedPairs(l:List[Int]):List[(Int,Int)] = {
    def makeAllUnsortedPairsWithHead(head:Int, tail:List[Int], toAppend:List[(Int,Int)]):List[(Int,Int)] = {
      tail match{
        case other :: newTail => makeAllUnsortedPairsWithHead(head, newTail, (head,other) :: toAppend)
        case Nil => toAppend
      }
    }

    l match{
      case Nil => List.empty
      case head :: tail => makeAllUnsortedPairsWithHead(head,tail,makeAllUnsortedPairs(tail))
    }
  }

  override def toString: String = "3-optKK"
}
