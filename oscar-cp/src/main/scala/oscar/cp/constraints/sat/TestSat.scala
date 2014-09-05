package oscar.cp.constraints.sat

import scala.util.Random
import oscar.cp.core.CPBoolVar
import oscar.cp.modeling._
import oscar.cp.search.BinaryStaticOrderBranching
import oscar.algo.search.SearchStatistics

object Test2SAT extends App {
  
  val nIter = 20

  // Data
  val nLiterals = 1000
  val density = 20
  
  print("Generate the instance...")
  val instance = Generator2Sat.buildInstance(nLiterals, density)
  println(" ok")
  
  print("Compute SAT statistics...")
  val stats2Sat = for (iter <- 1 to nIter) yield {
    print(s" $iter")
    SolverSAT.test2Sat(instance, nLiterals)  
  }
  println(" ok")
  
  print("Compute CP statistics... ")
  val statsCP = for (iter <- 1 to nIter) yield {
    print(s" $iter")
    SolverSAT.testCP(instance, nLiterals)
  }
  println(" ok")
  
  val (meanSat, devSat) = StatUtil.computeTimeStats(stats2Sat)
  val solsSat = stats2Sat.head.nSols
  val nodesSat = stats2Sat.head.nNodes
  
  val (meanCP, devCP) = StatUtil.computeTimeStats(statsCP)
  val solsCP = statsCP.last.nSols
  val nodesCP = statsCP.last.nNodes
  
  println("\nalgo\tmean\tdev\tsols\tnodes")
  println(s"2SAT\t$meanSat\t$devSat\t$solsSat\t$nodesSat")
  println(s"CP\t$meanCP\t$devCP\t$solsCP\t$nodesCP")
}

object Generator2Sat {
  def buildInstance(graphSize: Int, density: Int, seed: Int = 0) = {
    val rand = new Random(seed)
    val edges =
      for (
        i <- 0 until graphSize;
        j <- 0 until graphSize;
        rev <- Array(true, false);
        if (rand.nextInt(100) < density)
      ) yield (i, j, rev)
    edges.toArray
  }
}

object SolverSAT {
  
  def test2Sat(edges: Array[(Int, Int, Boolean)], nLiterals: Int): SearchStatistics = {
    implicit val solver = CPSolver()
    val literals = Array.fill(nLiterals)(BoolLiteral())
    val literalEdges = edges.map {
      case (i, j, rev) =>
        if (rev) (literals(i), literals(j).negation)
        else (literals(i), literals(j))
    }
    add(SatConstraint(literals ++ literals.map(_.negation), literalEdges))
    val booleans = literals.map(_.boolean)
    search(binaryStatic(booleans))
    start()   
  }

  def testCP(edges: Array[(Int, Int, Boolean)], nLiterals: Int): SearchStatistics = {
    implicit val solver = CPSolver()
    val booleans = Array.fill(nLiterals)(CPBoolVar())
    edges.foreach {
      case (i, j, rev) =>
        if (rev) add(booleans(i) | !booleans(j))
        else add(booleans(i) | booleans(j))
    }
    search(binaryStatic(booleans))
    start() 
  }
}

object StatUtil {
  def computeTimeStats(sample: Iterable[SearchStatistics]): (Double, Double) = {
    val mean = sample.map(_.time).sum.toDouble / sample.size
    val dev = math.sqrt(sample.map(s => math.pow(s.time - mean, 2)).sum / sample.size)
    (res(mean), res(dev))
  }
  def res(r: Double): Double = (r * 10).round.toDouble / 10
}