package oscar.examples.des

import oscar.des.montecarlo._

object PiApprox {
  val nIters = 100000
  val varsPerEval = 20000

  // Approximate the value of PI, using several random variables
  def evalFunc(rvList : List[AnyVal]) : Double = {
    def evalFunAcc(rvList : List[AnyVal], acc : Int) : Double = {
      rvList match {
        case Nil => acc
        case List(x) => acc
        case x :: y :: xs =>
          val x1 = x.asInstanceOf[Double]
          val y1 = y.asInstanceOf[Double]
          val isInCircle = if ((x1*x1 + y1*y1) > 1) { 0 } else { 1 }
          evalFunAcc(xs, acc+isInCircle)
      }
    }

    evalFunAcc(rvList, 0)
  }

  def aggrFunc(agg : Aggregator) : Double = 8*agg.sum/(varsPerEval*agg.size)

  // Run simulation based on concurrent futures
  def runFutures(): Unit = {
    val mcSim = new MonteCarloConcurrent(nIters, varsPerEval)
    mcSim.setEvaluationFunction(evalFunc)
    mcSim.setResultFunction(aggrFunc)
    println("Running concurrent futures simulation")
    val t0 = System.nanoTime
    val result = mcSim.runSimulation()
    val t1 = System.nanoTime
    println("Final Result = " + result)
    println("Elapsed time in seconds = " + (t1-t0)/1e9)
    println("Deviation from PI : " + (Math.PI - result))
  }

  // Run simulation based on recursive sequential algorithm
  def runRecursive(): Unit = {
    val mcSim = new MonteCarloSequential(nIters, varsPerEval)
    mcSim.setEvaluationFunction(evalFunc)
    mcSim.setResultFunction(aggrFunc)
    println("Running sequential recursive simulation")
    val t0 = System.nanoTime
    val result = mcSim.runSimulation()
    val t1 = System.nanoTime
    println("Final Result = " + result)
    println("Elapsed time in seconds = " + (t1-t0)/1e9)
    println("Deviation from PI : " + (Math.PI - result))
  }

  // Run simulation based on sequential streams
  def runStream(): Unit = {
    val mcSim = new MonteCarloStream(nIters, varsPerEval)
    mcSim.setEvaluationFunction(evalFunc)
    mcSim.setResultFunction(aggrFunc)
    println("Running sequential stream simulation")
    val t0 = System.nanoTime
    val result = mcSim.runSimulation()
    val t1 = System.nanoTime
    println("Final Result = " + result)
    println("Elapsed time in seconds = " + (t1-t0)/1e9)
    println("Deviation from PI : " + (Math.PI - result))
  }

  def main(args : Array[String]): Unit = {
    println(s"Simulation on $nIters iterations and $varsPerEval random variables")
    println("+------------------------------------------------+")
    runFutures()
    println("+------------------------------------------------+")
    runRecursive()
    println("+------------------------------------------------+")
    runStream()
    println("+------------------------------------------------+")
  }
}