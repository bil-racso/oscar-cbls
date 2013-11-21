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
  *     This code has been initially developed by Ghilain Florent.
  ******************************************************************************/

package oscar.cbls.routing.test
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.cbls.invariants.core.computation.{IntVar, Model}
import oscar.cbls.invariants.lib.logic.Routes
import scala.language.reflectiveCalls

class TestRouteTwoVehicles extends FunSuite with ShouldMatchers {

  def fixture =
    new {
      val UNROUTED = 12
      val ROUTE_ARRAY_UNROUTED = 2
      var nbPoints = 12
      var nbCars = 2
      val model = new Model(false,None,false,false)
      val next = Array.tabulate(nbPoints)(i => if(i<nbCars) new IntVar(model, i, nbPoints-1, i, "next" + i)
      else new IntVar(model, 0, nbPoints, i, "next" + i))
      // 0->1->2->3->4->5(->0)
      next(0):= 2
      next(2):= 3
      next(3):= 4
      next(4):= 5
      next(5):= 6
      next(6) :=0

      next(1):=7
      next(7):=8
      next(8):=9
      next(9):=10
      next(10):=11
      next(11):=1
      val routes = Routes.buildRoutes(next,nbCars)
      model.close()
    }

  test("Instantiation"){
    val f = fixture
    f.model.propagate()

    f.routes.RouteLength(0).value should be(6)
    f.routes.RouteLength(1).value should be(6)

    f.routes.LastInRoute(0).value should be(6)
    f.routes.LastInRoute(1).value should be(11)

    f.routes.PositionInRoute(0).value should be(0)
    f.routes.PositionInRoute(1).value should be(0)

    f.routes.RouteNr(0).value should be(0)
    f.routes.RouteNr(1).value should be(1)

    for(i <- 2 to 6){
      f.routes.PositionInRoute(i).value should be(i-1)
      f.routes.RouteNr(i).value should be(0)
    }
    for(i <- 7 to 11){
      f.routes.PositionInRoute(i).value should be(i-1-5)
      f.routes.RouteNr(i).value should be(1)
    }
  }

  test("move 5 to route nr 2"){
    val f = fixture
    f.next(4):=6
    f.next(5):= 9
    f.next(8):= 5
    f.model.propagate()

    f.routes.RouteLength(0).value should be(5)
    f.routes.RouteLength(1).value should be(7)

    f.routes.LastInRoute(0).value should be(6)
    f.routes.LastInRoute(1).value should be(11)

    f.routes.PositionInRoute(0).value should be(0)
    f.routes.PositionInRoute(1).value should be(0)
    for(i <- 2 to 6){
      if(i<5) f.routes.PositionInRoute(i).value should be(i-1)
      else if (i>5) f.routes.PositionInRoute(i).value should be(i-2)
    }
    f.routes.PositionInRoute(5).value should be(3)
    f.routes.RouteNr(5).value should be(1)
    for(i <- 7 to 11)
      if(i<9)
        f.routes.PositionInRoute(i).value should be(i-1-5)
      else
        f.routes.PositionInRoute(i).value should be(i-1-4)
  }

  test("swap segment 3-4 with segment 8-9"){
    val f = fixture
    f.next(2):=8
    f.next(9):=5
    f.next(7):=3
    f.next(4):=10
    f.model.propagate()

    f.routes.RouteLength(0).value should be(6)
    f.routes.RouteLength(1).value should be(6)

    f.routes.LastInRoute(0).value should be(6)
    f.routes.LastInRoute(1).value should be(11)

    f.routes.PositionInRoute(0).value should be(0)
    f.routes.PositionInRoute(1).value should be(0)
    for(i <- 2 to 6){
      if(i<3) f.routes.PositionInRoute(i).value should be(i-1)
      else if(i<5) {
        f.routes.PositionInRoute(i+5).value should be(i-1)
        f.routes.RouteNr(i+5).value should be(0)
      }
      else f.routes.PositionInRoute(i).value should be(i-1)
    }
    for(i <- 7 to 11)
      if(i<8)
        f.routes.PositionInRoute(i).value should be(i-1-5)
      else if(i<10){
        f.routes.PositionInRoute(i-5).value should be(i-1-5)
        f.routes.RouteNr(i-5).value should be(1)
      }
      else  f.routes.PositionInRoute(i).value should be(i-1-5)
  }

  test("unroute the last elements of both route"){
    val f = fixture
    f.next(6) := f.UNROUTED
    f.next(5) := 0

    f.next(11) := f.UNROUTED
    f.next(10) := 1
    f.model.propagate()

    f.routes.RouteLength(0).value should be(5)
    f.routes.RouteLength(1).value should be(5)

    f.routes.LastInRoute(0).value should be(5)
    f.routes.LastInRoute(1).value should be(10)

    f.routes.PositionInRoute(0).value should be(0)
    f.routes.PositionInRoute(1).value should be(0)
    for(i <- 2 to 6){
      if(i<6) f.routes.PositionInRoute(i).value should be(i-1)
      else {
        f.routes.PositionInRoute(i).value should be(f.UNROUTED)
        f.routes.RouteNr(i).value should be(f.ROUTE_ARRAY_UNROUTED)
      }
    }
    for(i <- 7 to 11){
      if(i<11)
        f.routes.PositionInRoute(i).value should be(i-1-5)
      else{
        f.routes.PositionInRoute(i-5).value should be(f.UNROUTED)
        f.routes.RouteNr(i-5).value should be(f.ROUTE_ARRAY_UNROUTED)
      }
    }
  }

  test("change order of elements in both route and unroute 9 and 10 from route 2"){
    val f = fixture
    f.next(6) := 5
    f.next(4) := 6
    f.next(5) := 0

    f.next(8) := 1
    f.next(11) := 8
    f.next(7) := 11
    f.next(9) := f.UNROUTED
    f.next(10) := f.UNROUTED

    f.model.propagate()

    f.routes.RouteLength(0).value should be(6)
    f.routes.RouteLength(1).value should be(4)

    f.routes.LastInRoute(0).value should be(5)
    f.routes.LastInRoute(1).value should be(8)

    f.routes.PositionInRoute(0).value should be(0)
    f.routes.PositionInRoute(1).value should be(0)
    for(i <- 2 to 6){
      if(i<5) f.routes.PositionInRoute(i).value should be(i-1)
      else  if(i==5) f.routes.PositionInRoute(i).value should be(i-1+1)
      else f.routes.PositionInRoute(i).value should be(i-1-1)
    }
    for(i <- 7 to 11){
      if(i<8)
        f.routes.PositionInRoute(i).value should be(i-1-5)
      else if(i<9)
        f.routes.PositionInRoute(i).value should be(3)
      else if(i<11){
        f.routes.PositionInRoute(i).value should be(f.UNROUTED)
        f.routes.RouteNr(i).value should be(f.ROUTE_ARRAY_UNROUTED)
      }
      else
        f.routes.PositionInRoute(i).value should be(2)
    }


  }
}
