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

import scala.math.pow
import scala.math.round
import scala.math.sqrt
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.Checkers
import oscar.cbls.invariants.core.computation.Model
import oscar.cbls.routing.initial.BestInsert
import oscar.cbls.routing.model.ClosestNeighborPointsHop
import oscar.cbls.routing.model.HopDistanceAsObjective
import oscar.cbls.routing.model.PenaltyForUnrouted
import oscar.cbls.routing.model.PositionInRouteAndRouteNr
import oscar.cbls.routing.model.UnroutedImpl
import oscar.cbls.routing.model.VRP
import oscar.cbls.routing.neighborhood.Swap
import oscar.cbls.routing.neighborhood.SearchZone
import oscar.cbls.routing.neighborhood.OnePointMove

/**
 * The tests marked with a star (*) require the assertion mechanism of IntVar in ComputationStructure file, which
 * verifies the domain of a variable variable.
 * These tests (with star) show the lack of robustness of the current framework.
 */
class TestMove extends FunSuite with ShouldMatchers with Checkers {
  ignore("A node can be cut.") {
    (f: MoveFixture) =>
      val cutNode = f.randomCutNodeAfter()
      f.commitPropagate

      f.mainRouteLength should be(f.nbNodes - 1)
      checkUnrouted(f, cutNode.start :: Nil)
  }

  ignore("A segment can be cut.") {
    (f: MoveFixture) =>
      val (initLength, cutSeg, segLength, segNodes) = f.randomCut
      f.commitPropagate

      f.mainRouteLength should be(initLength - segLength)
      checkUnrouted(f, segNodes)
  }

  // FIXME: sometimes fails
  ignore("A segment and a node can be cut.") {
    (f: MoveFixture) =>
      val (initLength, cutSeg, segLength, segNodes) = f.randomCut
      println(segNodes)
      val cutNode = f.randomCutNodeAfter((n: Int) =>
        !segNodes.contains(n) && !segNodes.contains(f.vrp.next(n).value))
      println("segNodes.contains(" + cutNode + "): " + segNodes.contains(cutNode))
      println("segNodes.contains(" + f.vrp.next(cutNode.start).value + "): " + segNodes.contains(f.vrp.next(cutNode.start).value))
      f.commitPropagate

      f.mainRouteLength should be(initLength - segLength - 1)
      checkUnrouted(f, cutNode.start :: segNodes)
  }

  ignore("Non disjoint segments cannot be cut.") {
    (f: MoveFixture) =>
      evaluating {
        val cutSeg07 = f.vrp.cut(0, 7)
        val cutSeg94 = f.vrp.cut(9, 4)
        f.vrp.unroute(cutSeg07)
        f.vrp.unroute(cutSeg94)
        f.commitPropagate
      } should produce[ArrayIndexOutOfBoundsException]
  }

  ignore("Cut and insert is allright.") {
    (f: MoveFixture) =>
      val (initLength, cutSeg, segLength, segNodes) = f.cut(0, 8)
      f.cutNodeAfter(4)
      f.commitPropagate

      val seg = f.vrp.segmentFromUnrouted(8)
      f.vrp.insert(seg, 7)
      f.commitPropagate

      f.mainRouteLength should be(initLength - segLength + 1)
      f.vrp.routes.RouteNr(8).value should be(0)
      f.vrp.routes.PositionInRoute(8).value should be(f.vrp.routes.PositionInRoute(7).value + 1)
  }

  ignore("Cannot insert routed node.") {
    (f: MoveFixture) =>
      evaluating {
        val (initLength, cutSeg, segLength, segNodes) = f.cut(0, 8)
        f.commitPropagate
        f.vrp.insert(cutSeg, 0)
        f.commitPropagate
      } should produce[AssertionError]
  }

  // TODO make this one generic
  ignore("A segment can be reversed.") {
    (f: MoveFixture) =>
      val (initLength, cutSeg, segLength, segNodes) = f.cut(0, 1)
      val revSeg = f.vrp.reverse(cutSeg)
      f.vrp.insert(revSeg, 0)
      f.commitPropagate

      f.mainRouteLength should be(initLength)
      for (i <- 0 to 9) f.vrp.routes.RouteNr(i).value should be(0)
      for (i <- 1 to 9) f.vrp.routes.PositionInRoute(i).value should be(f.vrp.routes.PositionInRoute((i + 1) % 9).value + 1)
  }

  ignore("A segment cannot be inserted after an unrouted node.") {
    (f: MoveFixture) =>
      val cutNode = f.randomCutNodeAfter()
      f.commitPropagate
      val (initLength, cutSeg, segLength, segNodes) = f.randomCut
      f.vrp.insert(cutSeg, cutNode.end)
      f.commitPropagate
  }

  ignore("A node cannot be inserted after an unrouted node.") {
    (f: MoveFixture) =>
      val cutNode = f.randomCutNodeAfter()
      f.commitPropagate
      val cutNode2 = f.randomCutNodeAfter()
      f.vrp.insert(cutNode2, cutNode.end)
      f.commitPropagate
  }

//  moveTest("A one point move can be done.", 1, true) {
  ignore("") {
    (f: MoveFixture) =>
      val pos = f.vrp.nodes.map((n: Int) => f.vrp.routes.PositionInRoute(n).value)
      val initLength = f.mainRouteLength
      val relevantNeighbors = (n: Int) => f.vrp.nodes
      OnePointMove.firstImprovingMove(
        SearchZone(relevantNeighbors, f.vrp.nodes.iterator, f.vrp)) match {
          case Some(m) => {
            m.isInstanceOf[OnePointMove] should be(true)
            val move = m.asInstanceOf[OnePointMove]
            println("predOfMovedPoint: " + move.predOfMovedPoint)
            println("insertionPoint: " + move.insertionPoint)
            val movedPoint = f.vrp.next(move.predOfMovedPoint).value
            val destPoint = f.vrp.next(move.insertionPoint).value
            println("Will insert node " + movedPoint + " after " + move.insertionPoint)
            m.doMove
            f.mainRouteLength should be(initLength)
            for (i <- f.vrp.nodes) {
              if (i == movedPoint) f.vrp.routes.PositionInRoute(i).value should be(pos(destPoint))
              else f.vrp.routes.PositionInRoute(i).value should be(pos(i))
            }
          }
          case None => assert(false)
        }
  }

  moveTest("Swap two points.", 1) {
    (f: MoveFixture) =>
      val pos = f.vrp.nodes.map((n: Int) => f.vrp.routes.PositionInRoute(n).value)
      val initLength = f.mainRouteLength
      val relevantNeighbors = (n: Int) => f.vrp.nodes
      Swap.firstImprovingMove(
        SearchZone(relevantNeighbors, f.vrp.nodes.iterator, f.vrp)) match {
          case Some(m) => {
            m.isInstanceOf[Swap] should be(true)
            val swap = m.asInstanceOf[Swap]
            println("A move was found ! : " + swap)
            val fst = f.vrp.next(swap.fstPred).value
            val snd = f.vrp.next(swap.sndPred).value
//            println("Will swap nodes " + fst + " and " + snd)
            m.doMove
            f.mainRouteLength should be(initLength)
            for (i <- f.vrp.nodes) {
              if (i == fst) f.vrp.routes.PositionInRoute(fst).value should be(pos(snd))
              else if (i == snd) f.vrp.routes.PositionInRoute(snd).value should be(pos(fst))
              else f.vrp.routes.PositionInRoute(i).value should be(pos(i))
            }

          }
          case None => assert(false)
        }
  }
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

  def moveTest(
    name: String,
    verbose: Int = 0,
    randomWeight: Boolean = false)(moveFun: MoveFixture => Unit): Unit = {
    test(name) {
      val f = new MoveFixture(verbose, randomWeight)

      if (verbose > 0) {
        println(f.vrp)
      }
      moveFun(f)

      if (verbose > 0) {
        println(f.vrp)
      }
    }
  }

  def checkUnrouted(f: MoveFixture, l: List[Int]) = l.foreach {
    (n: Int) =>
      f.vrp.routes.RouteNr(n).value should be(f.ROUTE_ARRAY_UNROUTED)
      f.vrp.routes.PositionInRoute(n).value should be(f.nbNodes)
  }
}

class MoveFixture(
  val verbose: Int = 0,
  val randomWeight: Boolean = false,
  val nbNodes: Int = 10,
  val nbVehicules: Int = 1) {

  val ROUTE_ARRAY_UNROUTED = 1

  val abscissa = Array.iterate(0, nbNodes)(_ + 1)
  val ordinate: Array[Int] =
    if (randomWeight) {
      Gen.containerOfN[Array, Int](nbNodes, Gen.choose(0, 100)).sample.get
    } else {
      Array.fill(nbNodes)(0)
    }
  for (i <- 0 to ordinate.length - 1) print(ordinate(i) + " ")
  println()
  val matrix = getDistanceMatrix(abscissa, ordinate)
  val model: Model = new Model(false, None, false, false)

  val vrp = new VRP(nbNodes, nbVehicules, model) with HopDistanceAsObjective with PositionInRouteAndRouteNr with ClosestNeighborPointsHop with UnroutedImpl with PenaltyForUnrouted

  vrp.addObjectiveTerm(vrp.UnroutedPenalty)
  vrp.setUnroutedPenaltyWeight(10000)
  vrp.installCostMatrix(matrix)
  model.close()
  model.propagate()
  BestInsert(vrp)
  // 0 -> 1 -> 2 -> ... -> nbNodes - 1 (-> 0)
  // or
  // 0 -> nbNodes - 1 -> ... -> 2 -> 1 (-> 0)
  model.propagate()

  def mainRouteLength = vrp.routes.RouteLength(0).value

  /**
   * Gives a distance matrix by entering the abscissa and
   * ordinates of points in the plan.
   */
  def getDistanceMatrix(abscissa: Array[Int], ordinate: Array[Int]): Array[Array[Int]] = {
    val N = abscissa.length
    Array.tabulate(N, N)((i, j) => round(sqrt((pow(abscissa(i) - abscissa(j), 2)
      + pow(ordinate(i) - ordinate(j), 2)).toFloat)).toInt)
  }

  def genNodeOpt(constraint: Int => Boolean = (_: Int) => true): Option[Int] = {
    (Gen.choose(0, nbNodes - 1) suchThat constraint).sample
  }

  def genNodeToCutAfter(constraint: Int => Boolean = (_: Int) => true) = {
    def isNotADepotNextOf = (x: Int) => !vrp.isADepot(vrp.next(x).value)
    def aux: Int =
      (Gen.choose(0, nbNodes - 1)
        suchThat { (n: Int) => isNotADepotNextOf(n) && constraint(n) }).sample match {
          case Some(index) => index
          case None => aux
        }
    aux
  }

  def genSegToCut: (Int, Int) = {
    val beforeStart = genNodeOpt().get
    genNodeOpt(
      (n: Int) => n != vrp.next(beforeStart).value
        && (!vrp.isInstanceOf[PositionInRouteAndRouteNr]
          || vrp.asInstanceOf[PositionInRouteAndRouteNr].isASegment(beforeStart, n))) match {
        case Some(end) => (beforeStart, end)
        case None => genSegToCut
      }
  }

  def cutNodeAfter(beforeStart: Int, constraint: Int => Boolean = (_: Int) => true) = {
    println("Will cut after node number " + beforeStart)
    val cutNode = vrp.cutNodeAfter(beforeStart)
    vrp.unroute(cutNode)
    cutNode
  }

  def randomCutNodeAfter(constraint: Int => Boolean = (_: Int) => true) = {
    val beforeStart = genNodeToCutAfter(constraint)
    cutNodeAfter(beforeStart, constraint)
  }

  def cut(beforeStart: Int, end: Int) = {
    val initLength = vrp.routes.RouteLength(0).value
    println("Will cut from after node number " + beforeStart + " to " + end)
    val cutSeg = vrp.cut(beforeStart, end)
    val segLength = this.segLength(cutSeg.start, cutSeg.end)
    val segNodes = this.segNodes(cutSeg.start, cutSeg.end)
    vrp.unroute(cutSeg)
    (initLength, cutSeg, segLength, segNodes)
  }

  def randomCut = {
    val (beforeStart, end) = genSegToCut
    cut(beforeStart, end)
  }

  def segLength(start: Int, end: Int) = {
    def aux(start: Int, length: Int): Int = {
      if (start == end) length
      else aux(vrp.next(start).value, length + 1)
    }
    aux(start, 1)
  }

  def segNodes(start: Int, end: Int) = {
    def aux(start: Int, nodes: List[Int]): List[Int] = {
      if (start == end) (start :: nodes).reverse
      else aux(vrp.next(start).value, start :: nodes)
    }
    aux(start, Nil)
  }

  def commitPropagate = {
    vrp.commit(true)
    model.propagate()
  }
}
