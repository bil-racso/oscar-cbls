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

package oscar.cbls.test.routing

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
import oscar.cbls.routing.neighborhood.OnePointMove
import oscar.cbls.routing.neighborhood.SearchZone
import oscar.cbls.routing.neighborhood.Swap
import oscar.cbls.routing.neighborhood.TwoOptNeighborhood
import oscar.cbls.routing.neighborhood.TwoOptMove
import oscar.cbls.routing.neighborhood.TwoOptMove
import oscar.cbls.routing.neighborhood.TwoOptMove
import oscar.cbls.routing.neighborhood.ThreeOpt
import oscar.cbls.routing.model.Unrouted
import oscar.cbls.routing.model.VRPObjective
import oscar.cbls.routing.model.StrongConstraints
import oscar.cbls.routing.model.MoveDescription

/**
 * The tests marked with a star (*) require the assertion mechanism of IntVar in ComputationStructure file, which
 * verifies the domain of a variable variable.
 * These tests (with star) show the lack of robustness of the current framework.
 */
class TestMove extends FunSuite with ShouldMatchers with Checkers {
  test("A node can be cut.") {
    (f: MoveFixture) =>
      val cutNode = f.randomCutNodeAfter()
      f.vrp.commit(true)

      f.mainRouteLength should be(f.nbNodes - 1)
      checkUnrouted(f, cutNode.start :: Nil)
  }

  test("A segment can be cut.") {
    (f: MoveFixture) =>
      val (initLength, cutSeg, segLength, segNodes) = f.randomCut
      f.vrp.commit(true)

      f.mainRouteLength should be(initLength - segLength)
      checkUnrouted(f, segNodes)
  }

  // FIXME: sometimes fails
  test("A segment and a node can be cut.") {
    (f: MoveFixture) =>
      val (initLength, cutSeg, segLength, segNodes) = f.randomCut
      println(segNodes)
      val cutNode = f.randomCutNodeAfter((n: Int) =>
        !segNodes.contains(n) && !segNodes.contains(f.vrp.next(n).value))
      println("segNodes.contains(" + cutNode + "): " + segNodes.contains(cutNode))
      println("segNodes.contains(" + f.vrp.next(cutNode.start).value + "): " + segNodes.contains(f.vrp.next(cutNode.start).value))
      f.vrp.commit(true)

      f.mainRouteLength should be(initLength - segLength - 1)
      checkUnrouted(f, cutNode.start :: segNodes)
  }

  test("Non disjoint segments cannot be cut.") {
    (f: MoveFixture) =>
      evaluating {
        val cutSeg07 = f.vrp.cut(0, 7)
        val cutSeg94 = f.vrp.cut(9, 4)
        f.vrp.unroute(cutSeg07)
        f.vrp.unroute(cutSeg94)
        f.vrp.commit(true)
      } should produce[ArrayIndexOutOfBoundsException]
  }

  test("Cut and insert is allright.") {
    (f: MoveFixture) =>
      val (initLength, cutSeg, segLength, segNodes) = f.cut(0, 8)
      f.cutNodeAfter(4)
      f.vrp.commit(true)

      val seg = f.vrp.segmentFromUnrouted(8)
      f.vrp.insert(seg, 7)
      f.vrp.commit(true)

      f.mainRouteLength should be(initLength - segLength + 1)
      f.vrp.routes.routeNr(8).value should be(0)
      f.vrp.routes.positionInRoute(8).value should be(f.vrp.routes.positionInRoute(7).value + 1)
  }

  test("Cannot insert routed node.") {
    (f: MoveFixture) =>
      evaluating {
        val (initLength, cutSeg, segLength, segNodes) = f.cut(0, 8)
        f.vrp.commit(true)
        f.vrp.insert(cutSeg, 0)
        f.vrp.commit(true)
      } should produce[AssertionError]
  }

  // TODO make this one generic
  test("A segment can be reversed.") {
    (f: MoveFixture) =>
      val (initLength, cutSeg, segLength, segNodes) = f.cut(0, 1)
      val revSeg = f.vrp.reverse(cutSeg)
      f.vrp.insert(revSeg, 0)
      f.vrp.commit(true)

      f.mainRouteLength should be(initLength)
      for (i <- 0 to 9) f.vrp.routes.routeNr(i).value should be(0)
      for (i <- 1 to 9) f.vrp.routes.positionInRoute(i).value should be(f.vrp.routes.positionInRoute((i + 1) % 9).value + 1)
  }

  test("A segment cannot be inserted after an unrouted node.") {
    (f: MoveFixture) =>
      val cutNode = f.randomCutNodeAfter()
      f.vrp.commit(true)
      val (initLength, cutSeg, segLength, segNodes) = f.randomCut
      f.vrp.insert(cutSeg, cutNode.end)
      f.vrp.commit(true)
  }

  test("A node cannot be inserted after an unrouted node.") {
    (f: MoveFixture) =>
      val cutNode = f.randomCutNodeAfter()
      f.vrp.commit(true)
      val cutNode2 = f.randomCutNodeAfter()
      f.vrp.insert(cutNode2, cutNode.end)
      f.vrp.commit(true)
  }

  moveTest("The first improving point move is done correctly.", 1, true) {
    (f: MoveFixture) =>
      val relevantNeighbors = (n: Int) => f.vrp.nodes
      OnePointMove.firstImprovingMove(
        SearchZone(relevantNeighbors, f.vrp.nodes.iterator, f.vrp)) match {
          case Some(m) => {
            m.isInstanceOf[OnePointMove] should be(true)
            val move = m.asInstanceOf[OnePointMove]
            println("An improving move was found ! : " + move)
            val movedPoint = f.vrp.next(move.predOfMovedPoint).value
            val nextPoint = f.vrp.next(movedPoint).value
            val destPoint = f.vrp.next(move.insertionPoint).value
            m.doMove
            println("VRP after the move: " + f.vrp)
            f.mainRouteLength should be(f.initLength)
            f.vrp.routes.positionInRoute(0).value should be(0)
            for (i <- f.vrp.nodes) {
              if (i == move.predOfMovedPoint)
                f.vrp.next(i).value should be(nextPoint)
              else if (i == move.insertionPoint)
                f.vrp.next(i).value should be(movedPoint)
              else if (i == movedPoint)
                f.vrp.next(i).value should be(destPoint)
              else
                f.vrp.next(i).value should be(f.initNext(i))
            }
          }
          case None => assert(false, "No improving move found, try launching this test again...")
        }
  }

  moveTest("The best improving point move is done correctly.", 1, true) {
    (f: MoveFixture) =>
      val relevantNeighbors = (n: Int) => f.vrp.nodes
      OnePointMove.bestImprovingMove(
        SearchZone(relevantNeighbors, f.vrp.nodes.iterator, f.vrp)) match {
          case Some(m) => {
            m.isInstanceOf[OnePointMove] should be(true)
            val move = m.asInstanceOf[OnePointMove]
            println("An improving move was found ! : " + move)
            val movedPoint = f.vrp.next(move.predOfMovedPoint).value
            val nextPoint = f.vrp.next(movedPoint).value
            val destPoint = f.vrp.next(move.insertionPoint).value
            m.doMove
            println("VRP after the move: " + f.vrp)
            f.mainRouteLength should be(f.initLength)
            f.vrp.routes.positionInRoute(0).value should be(0)
            for (i <- f.vrp.nodes) {
              if (i == move.predOfMovedPoint)
                f.vrp.next(i).value should be(nextPoint)
              else if (i == move.insertionPoint)
                f.vrp.next(i).value should be(movedPoint)
              else if (i == movedPoint)
                f.vrp.next(i).value should be(destPoint)
              else
                f.vrp.next(i).value should be(f.initNext(i))
            }
          }
          case None => assert(false, "No improving move found, try launching this test again...")
        }
  }

  moveTest("The first improving swap is done correctly.", 1, true) {
    (f: MoveFixture) =>
      val relevantNeighbors = (n: Int) => f.vrp.nodes
      Swap.firstImprovingMove(
        SearchZone(relevantNeighbors, f.vrp.nodes.iterator, f.vrp)) match {
          case Some(m) => {
            m.isInstanceOf[Swap] should be(true)
            val swap = m.asInstanceOf[Swap]
            println("An improving move was found ! : " + swap)
            val fst = f.vrp.next(swap.fstPred).value
            val snd = f.vrp.next(swap.sndPred).value
            val fstNext = f.vrp.next(fst).value
            val sndNext = f.vrp.next(snd).value
            m.doMove
            println("VRP after the move: " + f.vrp)
            f.mainRouteLength should be(f.initLength)
            f.vrp.routes.positionInRoute(0).value should be(0)
            for (i <- f.vrp.nodes) {
              if (i == swap.fstPred)
                f.vrp.next(i).value should be(snd)
              else if (i == snd)
                f.vrp.next(i).value should be(fstNext)
              else if (i == swap.sndPred)
                f.vrp.next(i).value should be(fst)
              else if (i == fst)
                f.vrp.next(i).value should be(sndNext)
              else
                f.vrp.next(i).value should be(f.initNext(i))
            }
          }
          case None => assert(false, "No improving move found, try launching this test again...")
        }
  }

  // FIXME
  //  moveTest("The first improving swap 2 is done correctly.", 1, true) {
  //    (f: MoveFixture) =>
  //      val relevantNeighbors = (n: Int) => f.vrp.nodes
  //      swap((sz: SearchZone) => Swap.firstImprovingMove(sz))(f)
  //  }

  moveTest("The best improving swap is done correctly.", 1, true) {
    (f: MoveFixture) =>
      val relevantNeighbors = (n: Int) => f.vrp.nodes
      Swap.bestImprovingMove(
        SearchZone(relevantNeighbors, f.vrp.nodes.iterator, f.vrp)) match {
          case Some(m) => {
            m.isInstanceOf[Swap] should be(true)
            val swap = m.asInstanceOf[Swap]
            println("An improving move was found ! : " + swap)
            val fst = f.vrp.next(swap.fstPred).value
            val snd = f.vrp.next(swap.sndPred).value
            val fstNext = f.vrp.next(fst).value
            val sndNext = f.vrp.next(snd).value
            m.doMove
            println("VRP after the move: " + f.vrp)
            f.mainRouteLength should be(f.initLength)
            f.vrp.routes.positionInRoute(0).value should be(0)
            for (i <- f.vrp.nodes) {
              if (i == swap.fstPred)
                f.vrp.next(i).value should be(snd)
              else if (i == snd)
                f.vrp.next(i).value should be(fstNext)
              else if (i == swap.sndPred)
                f.vrp.next(i).value should be(fst)
              else if (i == fst)
                f.vrp.next(i).value should be(sndNext)
              else
                f.vrp.next(i).value should be(f.initNext(i))
            }
          }
          case None => assert(false, "No improving move found, try launching this test again...")
        }
  }

  // FIXME
  //  moveTest("The best improving swap 2 is done correctly.", 1, true) {
  //    (f: MoveFixture) =>
  //      val relevantNeighbors = (n: Int) => f.vrp.nodes
  //      swap((sz: SearchZone) => Swap.bestImprovingMove(sz))(f)
  //  }

  //  test("swap unrouted point 2 with 6"){
  //    val f = fixture
  //    f.vrp.remove(List((0,2))).foreach(t => t._1 := t._2)
  //
  //    evaluating{
  //      f.vrp.swap(1,2,5,6).foreach(t => t._1 := t._2)
  //    } should produce [AssertionError]
  //  }
  //
  //  test("swap the sames points (3)"){
  //    val f = fixture
  //
  //    evaluating{
  //      f.vrp.swap(2,3,2,3).foreach(t => t._1 := t._2)
  //    } should produce [AssertionError]
  //  }
  //

  moveTest("A basic two-opt move is done correctly.", 1, false, 4, 1,
    Array(0, 0, 2, 2), Array(0, 2, 0, 2),
    (vrp: VRP with VRPObjective with PositionInRouteAndRouteNr with MoveDescription) => {
      vrp.setCircuit(List(0, 1, 2, 3))
    }) {
      (f: MoveFixture) =>
        val relevantNeighbors = (n: Int) => f.vrp.nodes
        TwoOptNeighborhood.firstImprovingMove(
          SearchZone(relevantNeighbors, f.vrp.nodes.iterator, f.vrp)) match {
            case Some(m) => {
              m.isInstanceOf[TwoOptMove] should be(true)
              val move = m.asInstanceOf[TwoOptMove]
              println("An improving move was found ! : " + move)
              m.doMove

              check2OptMove(f, move)
            }
            case None => assert(false, "No improving move found, try launching this test again...")
          }
    }

  moveTest("A first two-opt move is done correctly.", 1, true) {
    (f: MoveFixture) =>
      val relevantNeighbors = (n: Int) => f.vrp.nodes
      TwoOptNeighborhood.firstImprovingMove(
        SearchZone(relevantNeighbors, f.vrp.nodes.iterator, f.vrp)) match {
          case Some(m) => {
            m.isInstanceOf[TwoOptMove] should be(true)
            val move = m.asInstanceOf[TwoOptMove]
            println("An improving move was found ! : " + move)
            m.doMove

            check2OptMove(f, move)
          }
          case None => assert(false, "No improving move found, try launching this test again...")
        }
  }

  moveTest("A best two-opt move is done correctly.", 1, true) {
    (f: MoveFixture) =>
      val relevantNeighbors = (n: Int) => f.vrp.nodes
      TwoOptNeighborhood.bestImprovingMove(
        SearchZone(relevantNeighbors, f.vrp.nodes.iterator, f.vrp)) match {
          case Some(m) => {
            m.isInstanceOf[TwoOptMove] should be(true)
            val move = m.asInstanceOf[TwoOptMove]
            println("An improving move was found ! : " + move)
            m.doMove

            check2OptMove(f, move)
          }
          case None => assert(false, "No improving move found, try launching this test again...")
        }
  }

  moveTest("A three-opt move is done correctly.", 1, true) {
    (f: MoveFixture) =>
      val relevantNeighbors = (n: Int) => f.vrp.nodes
      ThreeOpt.firstImprovingMove(
        SearchZone(relevantNeighbors, f.vrp.nodes.iterator, f.vrp)) match {
          case Some(m) => {
            m.isInstanceOf[ThreeOpt] should be(true)
            val move = m.asInstanceOf[ThreeOpt]
            println("An improving move was found ! : " + move)

            m.doMove

            check3OptMove(f, move)
          }
          case None => assert(false, "No improving move found, try launching this test again...")
        }
  }
  //
  //  test("3opt with 1-2, 4-5 and 7-8"){
  //    val f = fixture
  //    f.vrp.threeOptA(1,2,4,5,7,8).foreach(t => t._1 := t._2)
  //
  //    for(i<- 0 to 8){
  //      if(i<=1 || i >=8)
  //        f.vrp.routes.positionInRoute(i).value should be(i)
  //      else if(i<=7 && i>=5)
  //        f.vrp.routes.positionInRoute(i).value should be(i-3)
  //      else if(i>=2 && i<=4)
  //        f.vrp.routes.positionInRoute(i).value should be(i+3)
  //    }
  //  }
  //
  //  test("3opt (one reverse) with 1-2, 4-5 and 7-8"){
  //    val f = fixture
  //    f.vrp.threeOptB(1,2,4,5,7,8).foreach(t => t._1 := t._2)
  //
  //    for(i<- 0 to 8){
  //      if(i<=1 || i >=8)
  //        f.vrp.routes.positionInRoute(i).value should be(i)
  //      else if(i<=7 && i>=5)
  //        f.vrp.routes.positionInRoute(i).value should be(i-3)
  //      else if(i>=2 && i<4)
  //        f.vrp.routes.positionInRoute(i).value should be(f.vrp.routes.positionInRoute(i+1).value +1)
  //      else
  //        f.vrp.routes.positionInRoute(i).value should be(5)
  //    }
  //  }
  //
  //  test("3opt (two reverses) with 1-2, 4-5 and 7-8"){
  //    val f = fixture
  //    f.vrp.threeOptC(1,2,4,5,7,8).foreach(t => t._1 := t._2)
  //
  //    for(i<- 0 to 8){
  //      if(i<=1 || i >=8)
  //        f.vrp.routes.positionInRoute(i).value should be(i)
  //      else if(i<7 && i>=5)
  //        f.vrp.routes.positionInRoute(i).value should be(f.vrp.routes.positionInRoute(i+1).value +1)
  //      else if(i>=2 && i<4)
  //        f.vrp.routes.positionInRoute(i).value should be(f.vrp.routes.positionInRoute(i+1).value +1)
  //      else if (i==4)
  //        f.vrp.routes.positionInRoute(i).value should be(2)
  //      else
  //        f.vrp.routes.positionInRoute(i).value should be(5)
  //
  //    }
  //  }

  def moveTest(
    name: String,
    verbose: Int = 0,
    randomWeight: Boolean = false,
    nbNodes: Int = 10,
    nbVehicles: Int = 1,
    abscissa: Array[Int] = null,
    ordinate: Array[Int] = null,
    init: VRP with VRPObjective with PositionInRouteAndRouteNr with MoveDescription => Unit = BestInsert.apply)(moveFun: MoveFixture => Unit): Unit = {
    test(name) {
      val f = new MoveFixture(verbose, randomWeight, nbNodes, nbVehicles, abscissa, ordinate, init)

      if (verbose > 0) {
        println(f.vrp)
      }
      moveFun(f)

      //      if (verbose > 0) {
      //        println(f.vrp)
      //      }
    }
  }

  def swap(move: (oscar.cbls.routing.neighborhood.SearchZone => Option[oscar.cbls.routing.neighborhood.Move]))(f: MoveFixture): Unit = {
    val pos = f.vrp.nodes.map((n: Int) => f.vrp.routes.positionInRoute(n).value)
    val initLength = f.mainRouteLength
    val relevantNeighbors = (n: Int) => f.vrp.nodes
    move(
      SearchZone(relevantNeighbors, f.vrp.nodes.iterator, f.vrp)) match {
        case Some(m) => {
          m.isInstanceOf[Swap] should be(true)
          val swap = m.asInstanceOf[Swap]
          println("An improving move was found ! : " + swap)
          val fst = f.vrp.next(swap.fstPred).value
          val snd = f.vrp.next(swap.sndPred).value
          val fstNext = f.vrp.next(fst).value
          val sndNext = f.vrp.next(snd).value
          println("VRP after the move: " + f.vrp)
          f.mainRouteLength should be(f.initLength)
          f.vrp.routes.positionInRoute(0).value should be(0)
          for (i <- f.vrp.nodes) {
            if (i == swap.fstPred)
              f.vrp.next(i).value should be(snd)
            else if (i == snd)
              f.vrp.next(i).value should be(fstNext)
            else if (i == swap.sndPred)
              f.vrp.next(i).value should be(fst)
            else if (i == fst)
              f.vrp.next(i).value should be(sndNext)
            else
              f.vrp.next(i).value should be(f.initNext(i))
          }
        }
        case None => assert(false)
      }
  }

  def checkUnrouted(f: MoveFixture, l: List[Int]) = l.foreach {
    (n: Int) =>
      f.vrp.routes.routeNr(n).value should be(f.ROUTE_ARRAY_UNROUTED)
      f.vrp.routes.positionInRoute(n).value should be(f.nbNodes)
  }

  def check2OptMove(f: MoveFixture, move: TwoOptMove) = {
    val segStart = f.initNext(move.fstPred)
    val sndEdgeEnd = f.initNext(move.sndPred)
    println("VRP after the move: " + f.vrp)
    f.mainRouteLength should be(f.initLength)
    f.vrp.routes.positionInRoute(0).value should be(0)
    for (i <- f.vrp.nodes) {
      if (i == move.fstPred)
        f.vrp.next(i).value should be(move.sndPred)
      else if (i == segStart)
        f.vrp.next(i).value should be(sndEdgeEnd)
      else if (f.isInSeg(i, segStart, move)) f.vrp.nodes.find(f.initNext(_) == i) match {
        case None => assert(false, "This case should not occur.")
        case Some(initPred) => f.vrp.next(i).value should be(initPred)
      }
      else {
        f.vrp.next(i).value should be(f.initNext(i))
      }
    }
  }

  def check3OptMove(f: MoveFixture, move: ThreeOpt) {
    val segStart = f.initNext(move.fstEdgeStartPoint)
    val sndEdgeEnd = f.initNext(move.sndEdgeStartPoint)
    val trdEdgeEnd = f.initNext(move.trdEdgeStartPoint)
    println("VRP after the move: " + f.vrp)
    f.mainRouteLength should be(f.initLength)
    f.vrp.routes.positionInRoute(0).value should be(0)
    for (i <- f.vrp.nodes) {
      if (i == move.fstEdgeStartPoint)
        f.vrp.next(i).value should be(f.initNext(move.sndEdgeStartPoint))
      else if (i == move.sndEdgeStartPoint)
        f.vrp.next(i).value should be(f.initNext(move.trdEdgeStartPoint))
      else if (i == move.trdEdgeStartPoint)
        f.vrp.next(i).value should be(f.initNext(move.fstEdgeStartPoint))
      else {
        f.vrp.next(i).value should be(f.initNext(i))
      }
    }
  }
}

class MoveFixture(
  val verbose: Int = 0,
  val randomWeight: Boolean = false,
  val nbNodes: Int = 10,
  val nbVehicules: Int = 1,
  var abscissa: Array[Int] = null,
  var ordinate: Array[Int] = null,
  val init: VRP with VRPObjective with PositionInRouteAndRouteNr with MoveDescription => Unit = BestInsert.apply) {

  val ROUTE_ARRAY_UNROUTED = 1

  if (abscissa == null)
    abscissa = Array.iterate(0, nbNodes)(_ + 1)
  if (ordinate == null)
    ordinate =
      if (randomWeight) {
        Gen.containerOfN[Array, Int](nbNodes, Gen.choose(0, 100)).sample.get
      } else {
        Array.fill(nbNodes)(0)
      }
  val matrix = getDistanceMatrix(abscissa, ordinate)
  val model: Model = new Model(false, None, false, false)

  val vrp = new VRP(nbNodes, nbVehicules, model) with HopDistanceAsObjective with PositionInRouteAndRouteNr with ClosestNeighborPointsHop with UnroutedImpl with PenaltyForUnrouted with MoveDescription

  vrp.addObjectiveTerm(vrp.UnroutedPenalty)
  vrp.setUnroutedPenaltyWeight(10000)
  vrp.installCostMatrix(matrix)
  model.close()

  println(vrp)

  init(vrp)
  // 0 -> 1 -> 2 -> ... -> nbNodes - 1 (-> 0)
  // or
  // 0 -> nbNodes - 1 -> ... -> 2 -> 1 (-> 0)
  model.propagate()

  val initLength = mainRouteLength
  val initNext = vrp.nodes.map((n: Int) => vrp.next(n).value)
  val initPos = vrp.nodes.map((n: Int) => vrp.routes.positionInRoute(n).value)
  def mainRouteLength = vrp.routes.routeLength(0).value

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
    val initLength = vrp.routes.routeLength(0).value
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

  /**
   * To check points from next(segStart) to sndPred
   */
  def isInSeg(i: Int, segStart: Int, move: TwoOptMove): Boolean = {
    var cur = segStart
    while (cur != move.sndPred) {
      cur = initNext(cur)
      if (i == cur) return true
    }
    false
  }
}
