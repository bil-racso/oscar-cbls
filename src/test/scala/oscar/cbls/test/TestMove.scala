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
 *     This code has been initially developed by Ghilain Florent.
 * ****************************************************************************
 */

package oscar.cbls.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import math._
import oscar.cbls.invariants.core.computation.Model
import oscar.cbls.routing.model._
import oscar.cbls.routing.initial.BestInsert

/**
 * The tests marked with a star (*) require the assertion mechanism of IntVar in ComputationStructure file, which
 * verifies the domain of a variable variable.
 * These tests (with star) show the lack of robustness of the current framework.
 */
class TestMove extends FunSuite with ShouldMatchers {

  def fixture =
    new {
      /**
       * Gives a distance matrix by entering the abscissa and
       * ordinates of points in the plan.
       */
      def getDistanceMatrix(abscissa: Array[Int], ordinate: Array[Int]): Array[Array[Int]] = {
        val N = abscissa.length
        Array.tabulate(N, N)((i, j) => round(sqrt((pow(abscissa(i) - abscissa(j), 2)
          + pow(ordinate(i) - ordinate(j), 2)).toFloat)).toInt)
      }
      val UNROUTED = 9
      val ROUTE_ARRAY_UNROUTED = 1
      val V: Int = 1
      val N: Int = 9

      val matrix = getDistanceMatrix(Array(0, 1, 2, 3, 4, 5, 6, 7, 8), Array(0, 0, 0, 0, 0, 0, 0, 0, 0))
      val model: Model = new Model(false, None, false, false)

      val vrp = new VRP(N, V, model)
      with HopDistanceAsObjective
      with PositionInRouteAndRouteNr
      with ClosestNeighborPointsHop
      with UnroutedImpl
      with PenaltyForUnrouted
      vrp.addObjectiveTerm(vrp.UnroutedPenalty)
      vrp.setUnroutedPenaltyWeight(10000)
      vrp.installCostMatrix(matrix)
      model.close()
      model.propagate()
      BestInsert(vrp)
      // 0 -> 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 8 (-> 0)
      // or
      // 0 -> 8 -> 7 -> 6 -> 5 -> 4 -> 3 -> 2 -> 1 (-> 0)
      model.propagate()
    }

  test("A node can be cut.") {
    val f = fixture
    
    println("before:\n" + f.vrp)
    
    val cutNode = f.vrp.cutNodeAfter(8)
    f.vrp.unroute(cutNode)
    f.vrp.commit(true)
    f.model.propagate()

    println("after:\n" + f.vrp)

    f.vrp.routes.RouteLength(0).value should be(8)
    f.vrp.routes.RouteNr(7).value should be(f.ROUTE_ARRAY_UNROUTED)
    f.vrp.routes.PositionInRoute(7).value should be(f.UNROUTED)
  }
  
//    test("remove points of 1-2"){
//      val f = fixture
//      val cutSeg02 = f.vrp.cut(0, 2)
//      f.vrp.unroute(cutSeg02)
////      f.vrp.remove(List((0,2))).foreach(t => t._1 := t._2)
//      f.model.propagate()
//  
//      f.vrp.routes.RouteLength(0).value should be(7)
//      f.vrp.routes.RouteNr(1).value should be(f.ROUTE_ARRAY_UNROUTED)
//      f.vrp.routes.RouteNr(2).value should be(f.ROUTE_ARRAY_UNROUTED)
//      f.vrp.routes.PositionInRoute(1).value should be(f.UNROUTED)
//      f.vrp.routes.PositionInRoute(2).value should be(f.UNROUTED)
//    }
//  
//    test("remove points of 1-2 and 5"){
//      val f = fixture
//      val cutSeg02 = f.vrp.cut(0, 2)
//      f.vrp.unroute(cutSeg02)
//      val cutNode5 = f.vrp.cut(4, 5)
//      f.vrp.unroute(cutNode5)
////      f.vrp.remove(List((0,2),(4,5))).foreach(t => t._1 := t._2)
//      f.model.propagate()
//  
//      f.vrp.routes.RouteLength(0).value should be(6)
//      f.vrp.routes.RouteNr(1).value should be(f.ROUTE_ARRAY_UNROUTED)
//      f.vrp.routes.RouteNr(2).value should be(f.ROUTE_ARRAY_UNROUTED)
//      f.vrp.routes.RouteNr(5).value should be(f.ROUTE_ARRAY_UNROUTED)
//      f.vrp.routes.PositionInRoute(1).value should be(f.UNROUTED)
//      f.vrp.routes.PositionInRoute(2).value should be(f.UNROUTED)
//      f.vrp.routes.PositionInRoute(5).value should be(f.UNROUTED)
//    }
//  
//    test("* remove points of an absent segment (3-1)"){
//      val f = fixture
//      evaluating{
//        val cutSeg21 = f.vrp.cut(2,1)
//        f.vrp.unroute(cutSeg21)
////        f.vrp.remove(List((2,1))).foreach(t => t._1 := t._2)
//        f.model.propagate()
//      } should produce[AssertionError]
//    }
//  
//    test("remove points of non disjoints segments (1-2 and 3-4)"){
//      val f = fixture
//      evaluating{
//        val cutSeg02 = f.vrp.cut(0, 2)
//        val cutSeg24 = f.vrp.cut(2, 4)
//        f.vrp.unroute(cutSeg02)
//        f.vrp.unroute(cutSeg24)
////        f.vrp.remove(List((0,2),(2,4))).foreach(t => t._1 := t._2)
//        f.model.propagate()
//      } should produce[ArrayIndexOutOfBoundsException]
//    }
//  
//    test("instead removing points of non disjoints segments, remove their union (1-4)"){
//      val f = fixture
//      val cutSeg04 = f.vrp.cut(0, 4)
//      f.vrp.unroute(cutSeg04)
////      f.vrp.remove(List((0,4))).foreach(t => t._1 := t._2)
//      f.model.propagate()
//    }
  //
  //
  //  test("insert 2 after 3"){
  //    val f = fixture
  //    f.vrp.remove(List((0,2),(4,5))).foreach(t => t._1 := t._2)
  //    f.model.propagate()
  //    // 1,2 and 5 unrouted
  //
  //    f.vrp.add(3,2).foreach(t => t._1 := t._2)
  //    f.model.propagate()
  //
  //    f.vrp.routes.RouteLength(0).value should be(7)
  //    f.vrp.routes.RouteNr(2).value should be(0)
  //
  //    f.vrp.routes.PositionInRoute(2).value should be(f.vrp.routes.PositionInRoute(3).value + 1)
  //  }
  //
  //  test("insert a point already routed"){
  //    val f = fixture
  //    evaluating{
  //      f.vrp.add(3,2).foreach(t => t._1 := t._2)
  //      f.model.propagate()
  //    } should produce [AssertionError]
  //  }
  //
  //  test("reverse the route"){
  //    val f = fixture
  //    f.vrp.reverse(0,8).foreach(t => t._1 := t._2)
  //    f.vrp.Next(0):=8
  //    f.model.propagate()
  //
  //    f.vrp.routes.RouteLength(0).value should be(9)
  //    for(i <- 0 to 8) f.vrp.routes.RouteNr(i).value should be(0)
  //    for(i <- 1 to 8) f.vrp.routes.PositionInRoute(i).value should be(f.vrp.routes.PositionInRoute((i+1)%9).value + 1)
  //  }
  //
  //  test("reverse the route segment 2-6 and reattach it correctly"){
  //    val f = fixture
  //    f.vrp.reverse(2,6).foreach(t => t._1 := t._2)
  //    f.vrp.Next(1):=6
  //    f.vrp.Next(2):=7
  //    f.model.propagate()
  //
  //    f.vrp.routes.RouteLength(0).value should be(9)
  //    for(i <- 2 to 5) f.vrp.routes.PositionInRoute(i).value should be(f.vrp.routes.PositionInRoute((i+1)%9).value + 1)
  //
  //  }
  //
  //  test("* reverse the route segment 2-6 and reattach it badly"){
  //    val f = fixture
  //    f.vrp.reverse(2,6).foreach(t => t._1 := t._2)
  //    evaluating{
  //      f.model.propagate()
  //
  //      f.vrp.routes.RouteLength(0).value should be(9)
  //      for(i <- 2 to 5) f.vrp.routes.PositionInRoute(i).value should be(f.vrp.routes.PositionInRoute((i+1)%9).value + 1)
  //    } should produce [AssertionError]
  //  }
  //
  //  test("move 1 after 2"){
  //    val f = fixture
  //    f.vrp.moveTo(0,1,2).foreach(t => t._1 := t._2)
  //    f.model.propagate()
  //
  //    f.vrp.routes.RouteLength(0).value should be(9)
  //    for(i<- 0 to 8){
  //      if(i==1)
  //        f.vrp.routes.PositionInRoute(i).value should be(2)
  //      else if(i==2)
  //        f.vrp.routes.PositionInRoute(i).value should be(1)
  //      else
  //        f.vrp.routes.PositionInRoute(i).value should be(i)
  //    }
  //  }
  //
  //  test("move segment 1-4 after 8"){
  //    val f = fixture
  //    f.vrp.moveTo(0,4,8).foreach(t => t._1 := t._2)
  //
  //    f.model.propagate()
  //
  //    f.vrp.routes.RouteLength(0).value should be(9)
  //    for(i<- 0 to 8){
  //      if(i==0)
  //        f.vrp.routes.PositionInRoute(i).value should be(0)
  //      else if(i<=4){
  //        f.vrp.routes.PositionInRoute(i).value should be(i+4)
  //      }
  //      else if(i<=8)
  //        f.vrp.routes.PositionInRoute(i).value should be(i-4)
  //    }
  //  }
  //
  //  test("move point 1 (segment 1-1) after an unrouted node (2)"){
  //    val f = fixture
  //    f.vrp.remove(List((1,2))).foreach(t => t._1 := t._2)
  //    f.model.propagate()
  //    // unroute 2
  //
  //    evaluating{
  //      f.vrp.moveTo(0,1,2).foreach(t => t._1 := t._2)
  //    } should produce[AssertionError]
  //  }
  //
  //  test("swap point 2 with point 6"){
  //    val f = fixture
  //
  //    f.vrp.swap(1,2,5,6).foreach(t => t._1 := t._2)
  //    f.vrp.routes.RouteLength(0).value should be(9)
  //    for(i<- 0 to 8){
  //      if(i==2)
  //        f.vrp.routes.PositionInRoute(i).value should be(6)
  //      else if(i==6)
  //        f.vrp.routes.PositionInRoute(i).value should be(2)
  //      else
  //        f.vrp.routes.PositionInRoute(i).value should be(i)
  //    }
  //  }
  //
  //  test("swap unrouted point 2 with 6"){
  //    val f = fixture
  //    f.vrp.remove(List((0,2))).foreach(t => t._1 := t._2)
  //    f.model.propagate()
  //
  //    evaluating{
  //      f.vrp.swap(1,2,5,6).foreach(t => t._1 := t._2)
  //      f.model.propagate()
  //    } should produce [AssertionError]
  //  }
  //
  //  test("swap the sames points (3)"){
  //    val f = fixture
  //
  //    evaluating{
  //      f.vrp.swap(2,3,2,3).foreach(t => t._1 := t._2)
  //      f.model.propagate()
  //    } should produce [AssertionError]
  //  }
  //
  //  test("2opt with 2-3 and 6-7"){
  //    val f = fixture
  //    f.vrp.twoOpt(2,3,6,7).foreach(t => t._1 := t._2)
  //    f.model.propagate()
  //
  //    f.vrp.routes.RouteLength(0).value should be(9)
  //    for(i<- 0 to 8){
  //      if(i<=2)
  //        f.vrp.routes.PositionInRoute(i).value should be(i)
  //      else if(i<6)
  //        f.vrp.routes.PositionInRoute(i).value should be(f.vrp.routes.PositionInRoute(i+1).value +1)
  //      else if(i==6)
  //        f.vrp.routes.PositionInRoute(i).value should be(f.vrp.routes.PositionInRoute(2).value +1)
  //      else
  //        f.vrp.routes.PositionInRoute(i).value should be(i)
  //    }
  //  }
  //
  //  test("3opt with 1-2, 4-5 and 7-8"){
  //    val f = fixture
  //    f.vrp.threeOptA(1,2,4,5,7,8).foreach(t => t._1 := t._2)
  //    f.model.propagate()
  //
  //    for(i<- 0 to 8){
  //      if(i<=1 || i >=8)
  //        f.vrp.routes.PositionInRoute(i).value should be(i)
  //      else if(i<=7 && i>=5)
  //        f.vrp.routes.PositionInRoute(i).value should be(i-3)
  //      else if(i>=2 && i<=4)
  //        f.vrp.routes.PositionInRoute(i).value should be(i+3)
  //    }
  //  }
  //
  //  test("3opt (one reverse) with 1-2, 4-5 and 7-8"){
  //    val f = fixture
  //    f.vrp.threeOptB(1,2,4,5,7,8).foreach(t => t._1 := t._2)
  //    f.model.propagate()
  //
  //    for(i<- 0 to 8){
  //      if(i<=1 || i >=8)
  //        f.vrp.routes.PositionInRoute(i).value should be(i)
  //      else if(i<=7 && i>=5)
  //        f.vrp.routes.PositionInRoute(i).value should be(i-3)
  //      else if(i>=2 && i<4)
  //        f.vrp.routes.PositionInRoute(i).value should be(f.vrp.routes.PositionInRoute(i+1).value +1)
  //      else
  //        f.vrp.routes.PositionInRoute(i).value should be(5)
  //    }
  //  }
  //
  //  test("3opt (two reverses) with 1-2, 4-5 and 7-8"){
  //    val f = fixture
  //    f.vrp.threeOptC(1,2,4,5,7,8).foreach(t => t._1 := t._2)
  //    f.model.propagate()
  //
  //    for(i<- 0 to 8){
  //      if(i<=1 || i >=8)
  //        f.vrp.routes.PositionInRoute(i).value should be(i)
  //      else if(i<7 && i>=5)
  //        f.vrp.routes.PositionInRoute(i).value should be(f.vrp.routes.PositionInRoute(i+1).value +1)
  //      else if(i>=2 && i<4)
  //        f.vrp.routes.PositionInRoute(i).value should be(f.vrp.routes.PositionInRoute(i+1).value +1)
  //      else if (i==4)
  //        f.vrp.routes.PositionInRoute(i).value should be(2)
  //      else
  //        f.vrp.routes.PositionInRoute(i).value should be(5)
  //
  //    }
  //  }

}

