package oscar.cbls.test.neighborhood

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.Checkers
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.{CBLSSeqVar, Objective, Store, bestSlopeFirst, length, sum}
import oscar.cbls.business.routing.{DistanceHelper, VRP}
import oscar.cbls.core.computation.{CBLSSetVar, Store}
import oscar.cbls.core.search.{Best, EasyNeighborhoodMultiLevel, First, Neighborhood}
import oscar.examples.cbls.routing.RoutingMatrixGenerator
import oscar.cbls.business.routing._
import oscar.cbls._
import oscar.cbls.test.neighborhood.NeighborhoodTestBench.MockedVRP

class RoutingNeighborhoodTestSuite extends FunSuite with Checkers with Matchers{

  test("insertPointUnroutedFirst"){

    def check(previousObjective :Long, newObjective :Long,
              previousSequence :IntSequence, newSequence :IntSequence) : Unit = {

      // Since we add an unrouted node
      // And there is a big penalty on unrouted nodes
      // The new objective should be lower than the old objective

      previousObjective should be >= newObjective
      previousSequence.size should be <= newSequence.size
    }

    val (model,problem,obj,closestRelevantNeighbors,postFilter) = NeighborhoodTestBench.initTest(check)
    val insertPointNeighborhood =
      insertPointUnroutedFirst(
        problem.unrouted,
        ()=>problem.kFirst(6,d => closestRelevantNeighbors(d),postFilter),
        problem
      )

    model.close()

    val searchProcedure = bestSlopeFirst(List(insertPointNeighborhood))
    searchProcedure.doAllMoves(obj = obj)
  }

  test("insertPointRoutedFirst"){

    def check(previousObjective :Long, newObjective :Long,
              previousSequence :IntSequence, newSequence :IntSequence) : Unit = {


      previousObjective should be >= newObjective
      previousSequence.size should be <= newSequence.size
    }

    val (model,problem,obj,closestRelevantNeighbors,postFilter) = NeighborhoodTestBench.initTest(check)
    val insertPointNeighborhood =
      insertPointRoutedFirst(
        problem.routed,
        ()=>problem.kFirst(6,d => closestRelevantNeighbors(d),postFilter),
        problem
      )

    model.close()

    val searchProcedure = bestSlopeFirst(List(insertPointNeighborhood)) maxMoves 10
    searchProcedure.doAllMoves(obj = obj)
  }


  test("onePointMove"){
    def check(previousObjective :Long, newObjective :Long,
              previousSequence :IntSequence, newSequence :IntSequence) : Unit = {


      println("move")
    }

    val (model,problem,obj,closestRelevantNeighbors,postFilter) = NeighborhoodTestBench.initTest(check)
    val onePointNeighborhood =
      onePointMove(
        problem.routed,
        ()=>problem.kFirst(6,d => closestRelevantNeighbors(d),postFilter),
        problem
      )

    val insertPointNeighborhood =
      insertPointUnroutedFirst(
        problem.unrouted,
        ()=>problem.kFirst(6,d => closestRelevantNeighbors(d),postFilter),
        problem
      )

    model.close()

    val searchProcedure = bestSlopeFirst(List(insertPointNeighborhood,onePointNeighborhood)) maxMoves 10
    searchProcedure.doAllMoves(obj = obj)
  }
}
