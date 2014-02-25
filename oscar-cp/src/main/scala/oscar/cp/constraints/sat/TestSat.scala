package oscar.cp.constraints.sat

import scala.util.Random
import oscar.cp.core.CPBoolVar
import oscar.cp.modeling._
import oscar.cp.search.BinaryStaticOrderBranching
import oscar.algo.search.SearchStatistics

object Test2SAT extends App {
  
  val nIter = 50

  // Data
  val nLiterals = 500
  val edgePerMillion = 20000 // too little and the number of solutions explodes
  
  print("Generate the instance...")
  val instance = Generator2Sat.generateRandomEdges(nLiterals, edgePerMillion)
  println(" ok")
  
  print("Compute SAT statistics...")
  val stats2Sat = for (iter <- 1 to nIter) yield SolverSAT.test2Sat(instance, nLiterals)  
  println(" ok")
  
  print("Compute CP statistics...")
  val statsCP = for (iter <- 1 to nIter) yield SolverSAT.testCP(instance, nLiterals)
  println(" ok")
  
  val (meanSat, devSat) = StatUtil.computeTimeStats(stats2Sat)
  val (meanCP, devCP) = StatUtil.computeTimeStats(statsCP)
  
  println
  println("mean\tdev")
  println(s"$meanSat\t$devSat")
  println(s"$meanCP\t$devCP")
}

object Generator2Sat {
  def generateRandomEdges(graphSize: Int, edgePerMillion: Int, seed: Int = 0) = {
    val rand = new Random(seed)
    val edges =
      for (
        i <- 0 until graphSize;
        j <- 0 until graphSize;
        rev <- Array(true, false);
        if (rand.nextInt(1000000) < edgePerMillion)
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
    add(new SatConstraint(literals ++ literals.map(_.negation), literalEdges))
    val booleans = literals.map(_.boolean)
    search(binaryStatic(booleans))
    start()   
  }

  def testCP(edges: Array[(Int, Int, Boolean)], nLiterals: Int): SearchStatistics = {
    implicit val solver = CPSolver()
    val booleans = Array.tabulate(nLiterals) { i => CPBoolVar() }
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