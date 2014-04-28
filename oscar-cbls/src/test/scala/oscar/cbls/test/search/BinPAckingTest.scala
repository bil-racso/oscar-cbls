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

package oscar.cbls.test.search

import oscar.cbls.search.binPacking._
import oscar.cbls.modeling.CBLSModel
import oscar.cbls.search.binPacking.JumpSwapItems

/**
 * Created by rdl on 24/04/2014.
 */
object BinPackingTest extends CBLSModel with App{

  //the index is the first element of the couple
  def indexList(l:List[Int]):List[(Int,Int)] = null

  val itemSizes = List(20, 5, 6, 7, 3, 5, 9, 7, 3, 5)

  val binSizes = List(20, 20, 30)

  val problem = BinPackingProblem(itemSizes,binSizes,s, c, 0)

  s.close()

  val x =  MoveItem(problem) exhaustBack SwapItems(problem) orElse JumpSwapItems(problem)
  x.verbose = true
  x.doAllImprovingMoves(100)

  println(problem)
}
