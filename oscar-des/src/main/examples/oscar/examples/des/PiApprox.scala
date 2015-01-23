package oscar.examples.des

import oscar.des.montecarlo._

object PiApprox {
  val nIters = 10000
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

  // Run simulation based on recursive sequential algorithm
  def runMonteCarlo(): Unit = {
    val mcSim = new MonteCarlo(nIters, varsPerEval, evaluationFunction = evalFunc, resultFunction = aggrFunc)
    println("Running sequential recursive simulation")
    val t0 = System.nanoTime
    val result = mcSim.runSimulation()
    val t1 = System.nanoTime
    println("Final Result = " + result)
    println("Elapsed time in seconds = " + (t1-t0)/1e9)
    println("Deviation from PI : " + (Math.PI - result))
  }


  def main(args : Array[String]): Unit = {
    println(s"Pi Approximation with Monte Carlo method on $nIters iterations and $varsPerEval random variables")
    println("+------------------------------------------------+")
    runMonteCarlo()
    println("+------------------------------------------------+")
  }
}