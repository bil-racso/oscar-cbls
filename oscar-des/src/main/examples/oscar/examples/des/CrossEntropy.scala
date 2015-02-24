package oscar.examples.des

import java.io.File
import java.util.Scanner

import oscar.des.engine.NumberGenerator
import oscar.des.montecarlo._
import JSci.maths.statistics._

/**
 * Tests of Cross-Entropy method algorithms for Monte Carlo simulation.
 *
 * Created by go on 23/12/2014.
 */

/**
 * Test of the CE method to optimize the function
 * e^(x-2)² + 0.8*e^(x+2)²
 */
object TestCE1 {
  // Generic "average" function over an array (tail recursive)
  def average(numbers : Array[Double], ne : Int, fn : (Double) => Double) : Double = {
    def average(i : Int, accSum : Double) : Double =
      if (i == ne)
        accSum / i
      else
        average(i+1, accSum+fn(numbers(i)))

    average(0, 0.0)
  }

  // Mean of an array of numbers (as average)
  def mean(numbers : Array[Double], ne : Int) : Double = average(numbers, ne, (x) => x)

  // Standard variance (defined as average of cartesian distances)
  def variance(numbers : Array[Double], mean : Double, ne : Int) : Double =
    average(numbers, ne, (x) => {val xm = x-mean; xm*xm})

  // The objective function
  def objFuncS(x : Double) : Double = {
    val xp2 = x+2
    val xm2 = x-2

    Math.exp(-(xm2*xm2)) + 0.8*Math.exp(-(xp2*xp2))
  }

  // Printing an array of double numbers
  def strDbArr(name : String, arr : Array[Double]): Unit = {
    for (i <- 0 until arr.length) {
      println(s" $name($i) = ${arr(i)}")
    }
  }

  // Given an array of tuples, get an array of all first elements
  // of each tuple
  def projX(tups : Array[(Double, Double)]) : Array[Double] = {
    val retArr = new Array[Double](tups.length)
    for (i <- 0 until retArr.length) {
      retArr(i) = tups(i)._1
    }

    retArr
  }

  def main(args : Array[String]): Unit = {
    val maxits = 100
    val epsilon = 1e-8
    val Ne = 10
    val N = 100
    var mu = -6D
    var sigma2 = 100D
    var t = 0

    // Notice this CE loop is imperative
    while ((t < maxits) && (sigma2 > epsilon)) {
      val RVs = new Array[DoubleRandomVar](N)
      val Xs = new Array[(Double, Double)](N)
      val pdf = new NormalDistribution(mu, sigma2)
      val ng = new NumberGenerator(pdf)
      for (i <- 0 until N) {
        RVs(i) = new DoubleRandomVar(s"RdV$i", ng)
        val rvariate = RVs(i).getValue
        Xs(i) = (rvariate, objFuncS(rvariate))
      }
      val sortedXs = projX(Xs.sortWith((x, y) => x._2 > y._2))
      // Updating probability distribution parameters (mu, sigma2)
      mu = mean(sortedXs, Ne)
      sigma2 = variance(sortedXs, mu, Ne)
      t += 1

      println("+-------------------------------+")
      println(s"Data in iteration $t")
      println("Sorted Xs =")
      strDbArr("sortedXs", sortedXs)
      println(s"New Mu = $mu")
      println(s"New Sigma^2 = $sigma2")
      val of = objFuncS(mu)
      println(s"Value of objective function for mu = $of")
    }

    println("+-------------------------------+")
    println(s"Ended with result Mu = $mu")
    val of = objFuncS(mu)
    println(s"Value of objective function for mu = $of")
  }
}

/**
 * Test of the CE method to "approximate" a "hidden n-bits" value
 */
object TestCE2 {
  // Problem parameters
  val n = 10     // number of bits
  val N = 100    // number of random samples
  val arr = 0.01 // quantile parameter

  // Variable for debug only
  var i = 0

  // Establish the initial probability of being 1 for each bit.
  // That probability is 0.5
  def initialProbList : List[Float] = {
    def initialProbList(i : Int, lsAcc : List[Float]) : List[Float] = {
      if (i == n)
        lsAcc
      else
        initialProbList(i+1, 0.5F :: lsAcc)
    }

    initialProbList(0, Nil)
  }

  // Get the "hidden n-bits value", randomly.
  def initialY : List[Float] = {
    def initialY(i : Int, rdV : DoubleRandomVar, lsAcc : List[Float]) : List[Float] = {
      if (i == n)
        lsAcc
      else {
        initialY(i+1, rdV, Math.round(rdV.getValue) :: lsAcc)
      }
    }

    val rdV = new DoubleRandomVar("RdV", new UniformDistribution(0,1))
    initialY(0, rdV, Nil)
  }

  // Generate N random samples of n-bits with their "evaluation". The evaluation
  // measure is how many bits in the sample don't correspond with bits in the hidden value.
  // The algorithm is tail recursive with accumulators
  def generateSamplesWithS(lsY : List[Float], probVector : List[Float]) : List[(List[Float], Float)] = {
    def generateSamplesWithS(lsY : List[Float],
                             probVector : List[Float],
                             i : Int,
                             lsAcc : List[(List[Float], Float)]) : List[(List[Float], Float)] = {
      def generateSampleWithS(lsY : List[Float],
                              probVector : List[Float],
                              lsAcc : List[Float],
                              sAcc : Float) : (List[Float], Float) = (probVector, lsY) match {
        case (Nil, Nil) => (lsAcc, n-sAcc)
        case (p :: ps, y :: ys) =>
          val rdVar = new DoubleRandomVar("DRdV", new BinomialDistribution(2, p))
          val rdVal = rdVar.getValue.toFloat
          generateSampleWithS(ys, ps, lsAcc :+ rdVal, sAcc + Math.abs(rdVal - y))
        // unreachable case, because probVector and lsY must have the same length.
        // should it be better to throw an exception instead ?
        case _ => (lsAcc, n-sAcc)
      }

      if (i == N)
        lsAcc
      else {
        generateSamplesWithS(lsY, probVector, i+1, generateSampleWithS(lsY, probVector, Nil, 0F)::lsAcc)
      }
    }

    generateSamplesWithS(lsY, probVector, 0, Nil)
  }

  // Determine the arr-quantile sample in list
  def arrQuantile(lsS : List[(List[Float], Float)]) : (List[Float], Float) = {
    val qtile = Math.ceil((1-arr)*N).toInt

    lsS(qtile-1)
  }

  // Compute the new parameters of the Binomial probability distribution for each bit,
  // based on the random samples and the arr-quantile sample
  def computeNewProbList(sLs : List[(List[Float], Float)], perfQtile : Float) : List[Float] = {
    def computeProbAcc(i : Int, j : Int, accSumPonderate : Float, accSumHigherPerfs : Float) : Float = {
      if (i == N)
        accSumPonderate / accSumHigherPerfs
      else {
        val sLs_i = sLs(i)
        val accPonderate = if ((sLs_i._2 >= perfQtile) && (sLs_i._1(j) == 1F)) 1F else 0F
        val accHigherPerf = if (sLs_i._2 >= perfQtile) 1F else 0F
        computeProbAcc(i+1, j, accSumPonderate+accPonderate, accSumHigherPerfs+accHigherPerf)
      }
    }

    val seqProb = for (j <- 0 until n) yield computeProbAcc(0, j, 0F, 0F)
    seqProb.toList
  }

  // Stop criterion for CE loop : the probability parameters are 0 or 1
  def isDegenerateBinaryList(ls : List[Float]) : Boolean = ls match {
    case Nil => true
    case p :: ps =>
      if ((p == 1F) || (p == 0F))
        isDegenerateBinaryList(ps)
      else
        false
  }

  // Main CE loop : notice this version is tail recursive with accumulators
  def crossEntropyLoop(probList : List[Float], y : List[Float]) : List[Float] = {
    // Auxiliary tail recursive function: an iteration is already performed
    def crossEntropyLoopAcc(probList : List[Float],
                            y : List[Float],
                            approxY : List[Float]) : List[Float] = {
      if (isDegenerateBinaryList(probList))
        approxY
      else {
        val sortedLs = generateSamplesWithS(y, probList).sortBy(tup => tup._2)
        val posQtile = arrQuantile(sortedLs)
        val newProbList = computeNewProbList(sortedLs, posQtile._2)
        /////////// Debug info ///////////
        i += 1
        println(s"Iteration : $i")
        println("----------")
        println(s"Sorted Ls = $sortedLs")
        println("----------")
        println(s"Arr Quantile = $posQtile")
        println("----------")
        println(s"New Prob List = $newProbList")
        println("----------")
        //////////////////////////////////
        crossEntropyLoopAcc(newProbList, y, posQtile._1)
      }
    }
    // Notice that one iteration is performed before calling the auxiliary
    // function.
    val sortedLs = generateSamplesWithS(y, probList).sortBy(tup => tup._2)
    val posQtile = arrQuantile(sortedLs)
    val newProbList = computeNewProbList(sortedLs, posQtile._2)
    /////////// Debug info ///////////
    i += 1
    println(s"Iteration : $i")
    println("----------")
    println(s"Sorted Ls = $sortedLs")
    println("----------")
    println(s"Arr Quantile = $posQtile")
    println("----------")
    println(s"New Prob List = $newProbList")
    println("----------")
    //////////////////////////////////
    crossEntropyLoopAcc(newProbList, y, posQtile._1)
  }

  def main (args: Array[String]) : Unit = {
    val p0 = initialProbList // list of O.5
    val Y = initialY         // Value to "devinate" : it's random
    //val Y = List(1F, 1F, 1F, 1F, 1F, 0F, 0F, 0F, 0F, 0F)
    val aY = crossEntropyLoop(p0, Y)

    println("----------")
    println(s"P0 = $p0")
    println("----------")
    println(s"Y = $Y")
    println("----------")
    println(s"Approx Y = $aY")
    println("----------")
  }
}

/**
 * Test of the CE method to solve the "Max Cut" optimization problem.
 */
object TestCE3 {
  // Problem parameters
  val nNodes = 5
  val nSamples = 16
  val rho = 0.1
  val costGraphMat = List(List(0F,1F,3F,5F,6F),
                          List(1F,0F,3F,6F,5F),
                          List(3F,3F,0F,2F,2F),
                          List(5F,6F,2F,0F,2F),
                          List(6F,5F,2F,2F,0F))
  // Variable for debug only
  var i = 0

  /**
   * Main Cross-Entropy loop, implemented as a (tail) recursive function
   *
   * @param probList the list of probabilities for a Node to be in the same
   *                 partition of node 1 (i.e. to be 1 in the cut vector)
   * @return the optimum cut vector
   */
  def crossEntropyLoop(probList : List[Float]) : List[Int] = {
    /**
     * Generic function to remove duplicates of a list (Tail recursive)
     *
     * @param ls list with possible duplicates
     * @tparam A Type of list elements
     * @return ls with all duplicate elements removed
     */
    def removeDup[A](ls : List[A]) : List[A] = {
      // Accumulate auxiliary function
      def removeDup(ls : List[A], lsAcc : List[A]) : List[A] = {
        def removeAllX(x : A, ls : List[A], lsAcc : List[A]) : List[A] = ls match {
          case Nil => lsAcc
          case y :: ys =>
            if (x == y) removeAllX(x, ys, lsAcc)
            else removeAllX(x, ys, lsAcc :+ y)
        }

        ls match {
          case Nil => lsAcc
          case x :: xs => removeDup(removeAllX(x, xs, Nil), lsAcc :+ x)
        }
      }

      removeDup(ls, Nil)
    }

    /**
     * Main Cross-Entropy loop, with a first iteration already done
     *
     * @param probList the list of probabilities for a Node to be in the same
     *                 partition of first node (i.e. to be 1 in the cut vector)
     * @param bestFound the candidate cut vector found in last iteration
     * @return the optimum cut vector
     */
    def crossEntropyLoopAcc(probList : List[Float],
                            bestFound : (List[Int], Float)) : List[Int] = {
      if (isDegenerateBinaryList(probList))
        bestFound._1
      else {
        val sortedLs = removeDup(generateSamplesWithS(probList).sortBy(tup => tup._2))
        val posQtile = rhoQuantile(sortedLs)
        val newProbList = computeNewProbList(sortedLs, posQtile._2)
        /////////// Debug info ///////////
        i += 1
        println(s"Iteration : $i")
        println("----------")
        println(s"Sorted Ls = $sortedLs")
        println("----------")
        println(s"Arr Quantile = $posQtile")
        println("----------")
        println(s"New Prob List = $newProbList")
        println("----------")
        //////////////////////////////////
        if (bestFound._2 > posQtile._2)
          crossEntropyLoopAcc(newProbList, bestFound)
        else
          crossEntropyLoopAcc(newProbList, posQtile)
      }
    }

    val sortedLs = removeDup(generateSamplesWithS(probList).sortBy(tup => tup._2))
    val posQtile = rhoQuantile(sortedLs)
    val newProbList = computeNewProbList(sortedLs, posQtile._2)
    /////////// Debug info ///////////
    i += 1
    println(s"Iteration : $i")
    println("----------")
    println(s"Sorted Ls = $sortedLs")
    println("----------")
    println(s"Arr Quantile = $posQtile")
    println("----------")
    println(s"New Prob List = $newProbList")
    println("----------")
    //////////////////////////////////
    crossEntropyLoopAcc(newProbList, posQtile)
  }

  /**
   * Compute the new probability parameters
   *
   * @param sLs a sorted list of tuples "cut vector - performance"
   * @param perfQtile performance of the rho-quantile sample
   * @return the list of updated probability parameters
   */
  def computeNewProbList(sLs : List[(List[Int], Float)], perfQtile : Float) : List[Float] = {
    def computeProbAcc(i : Int, j : Int, accSumPonderate : Float, accSumHigherPerfs : Float) : Float = {
      if (i == sLs.length)
        accSumPonderate / accSumHigherPerfs
      else {
        val sLs_i = sLs(i)
        val accPonderate = if ((sLs_i._2 >= perfQtile) && (sLs_i._1(j) == 1)) 1F else 0F
        val accHigherPerf = if (sLs_i._2 >= perfQtile) 1F else 0F
        computeProbAcc(i+1, j, accSumPonderate+accPonderate, accSumHigherPerfs+accHigherPerf)
      }
    }

    val seqProb = for (j <- 0 until nNodes) yield computeProbAcc(0, j, 0F, 0F)
    seqProb.toList
  }

  /**
   * Compute the rho-quantile of the random samples
   * @param lsS a list of tuples (sample, performance), sorted by performance
   * @return the (1-rho)*nNodes element of lsS
   */
  def rhoQuantile(lsS : List[(List[Int], Float)]) : (List[Int], Float) = {
    val qtile = Math.ceil((1-rho)*nNodes).toInt

    if (qtile > lsS.length)
      lsS(lsS.length - 1)
    else if (qtile == 0)
      lsS(qtile)
    else
      lsS(qtile-1)
  }

  /**
   * Computes the initial probability parameters for each node
   * to be in the same partition of first node (i.e. to be 1 in the cut vector)
   * @return a list beginning with 1 (first node is always in a cut
   *         vector) followed by 0.5s
   */
  def initialProbList : List[Float] = {
    def initialProbList(i : Int, lsAcc : List[Float]) : List[Float] = {
      if (i == nNodes)
        1F :: lsAcc
      else
        initialProbList(i+1, 0.5F :: lsAcc)
    }

    initialProbList(1, Nil)
  }

  /**
   * Generates a random sample cut vector
   * @param probVector list of probability parameters for a node
   *                   to be in the same partition as first node
   * @return the random cut vector according to the probability
   *         distribution
   */
  def generateSample(probVector : List[Float]) : List[Int] = {
    def generateSample(probVector : List[Float], lsAcc : List[Int]) : List[Int] = probVector match {
      case Nil => lsAcc
      case p :: ps =>
        val rdVar = new IntRandomVar("IRdV", new BinomialDistribution(2, p))
        val rdVal = rdVar.getValue
        generateSample(ps, lsAcc :+ rdVal)
    }

    generateSample(probVector, Nil)
  }

  /**
   * Generates nSamples of cut vectors according to the binomial distribution
   * parameters given by probVector
   * @param probVector list of probability parameters for a node
   *                   to be in the same partition as node 1
   * @return a list of tuples (vector cut, cost of vector cut)
   */
  def generateSamplesWithS(probVector : List[Float]) : List[(List[Int], Float)] = {
    def generateSamplesWithS(probVector : List[Float],
                             i : Int,
                             lsAcc : List[(List[Int], Float)]) : List[(List[Int], Float)] = {
      def generateSampleWithS(probVector : List[Float],
                              lsAcc : List[Int]) : (List[Int], Float) = probVector match {
        case Nil => (lsAcc, costCut(lsAcc))
        case p :: ps =>
          val rdVar = new IntRandomVar("IRdV", new BinomialDistribution(2, p))
          val rdVal = rdVar.getValue
          generateSampleWithS(ps, lsAcc :+ rdVal)
      }

      if (i == nSamples)
        lsAcc
      else {
        generateSamplesWithS(probVector, i+1, generateSampleWithS(probVector, Nil)::lsAcc)
      }
    }

    generateSamplesWithS(probVector, 0, Nil)
  }

  /**
   * Stop criterion for the CE loop : probability parameters are all 0 or 1
   * @param ls a list of float numbers
   * @return true iff each element of ls is 0 or 1
   */
  def isDegenerateBinaryList(ls : List[Float]) : Boolean = ls match {
    case Nil => true
    case p :: ps =>
      if ((p == 1F) || (p == 0F))
        isDegenerateBinaryList(ps)
      else
        false
  }

  /**
   * Computes the cost of a partition specified by a cut vector
   * @param cutVector a list of 1-0 numbers. cutVector(i) == 1 indicates that the
   *                  i-th node is in the same partition of first node.
   * @return the cost : that means the sum of cost of edges of nodes in
   *         partition 1 to nodes in partition 2 (note: cost matrix is symmetric)
   */
  def costCut(cutVector : List[Int]) : Float = {
    var accCost = 0F
    for (i <- 0 until nNodes) {
      if (cutVector(i) == 1) {
        for (j <- 0 until nNodes) {
          if (cutVector(j) == 0) {
            accCost += costGraphMat(i)(j)
          }
        }
      }
    }
    accCost
  }

  def main (args: Array[String]): Unit = {
    val p0 = initialProbList
    val cutV = generateSample(p0)
    println("Cost Matrix = " + costGraphMat)
    println("Cut Vector = " + cutV)
    println("Cost Cut Vector = " + costCut(cutV))
    val bestCut = crossEntropyLoop(p0)
    println("Best Cut Vector = " + bestCut)
    println("Cost Best Cut Vector = " + costCut(bestCut))
  }
}

/**
 * Test of the CE method to solve the Multi-Knapsack problem
 */
object TestCE4 {

  /**
   * Modeling a Multi-knapsack problem
   * @param nKnapsacks number of knapsacks
   * @param nObjects number of objects
   * @param objectWeights weights (benefits) of each object
   * @param knapsackCapacities capacities of each knapsack
   * @param constraints weight constraints for objects in knapsacks
   */
  case class MultiKnapsackProblem(nKnapsacks : Int,
                             nObjects : Int,
                             objectWeights : List[Int],
                             knapsackCapacities : List[Int],
                             constraints : List[List[Int]]) {
    val sumObjectWeights = objectWeights.fold(0)((a:Int, b:Int) => a+b)

    // Objective function
    def evaluateS(xs : List[Int]) : Int = {
      def ponderateWeight(weights : List[Int]) : Int = {
        var sum = 0
        for (i <- 0 until nObjects) {
          sum += weights(i)*xs(i)
        }
        sum
      }
      def violatedConstraints : Integer = {
        var sum = 0
        for (i <- 0 until nKnapsacks) {
          sum += (if (ponderateWeight(constraints(i)) > knapsackCapacities(i)) 1 else 0)
        }
        sum
      }

      ponderateWeight(objectWeights) - sumObjectWeights*violatedConstraints
    }
  }

  // CE method parameters
  val nSamples = 5000
  val rho = 0.02
  val threshold = 0.01
  // Hard-coded instance of a multi-knapsack problem
  val mkProblem = new MultiKnapsackProblem(2,
                                           28,
                                           List(1898, 440, 22507, 270, 14148, 3100, 4650, 30800,
    615, 4975, 1160, 4225, 510, 11880, 479, 440, 490, 330, 110, 560, 24355, 2885, 11748, 4550, 750,
    3720, 1950, 10500),
                                           List(600, 600),
                                           List(List(45, 0, 85, 150, 65, 95, 30, 0, 170, 0, 40, 25, 20,
    0, 0, 2, 5, 0, 0, 25, 0, 165, 0, 85, 0, 0, 0, 0, 100),
                                                List(30, 20, 125, 5, 80, 25, 35, 73, 12,
    15, 15, 40, 5, 10, 10, 12, 10, 9, 0, 20, 60, 40, 50, 36, 49, 40, 19, 150)))

  // Variable for debug info
  var i = 1

  def loadMKProblem(strFile : String) : Option[MultiKnapsackProblem] = {
    def loadMKProblem(sc : Scanner) : Option[MultiKnapsackProblem] = {
      def scanNInts(n : Int) : List[Int] = {
        def scanNInts(i : Int, n : Int, lsAcc : List[Int]) : List[Int] = {
          if (i == n)
            lsAcc
          else
            scanNInts(i+1, n, lsAcc :+ sc.nextInt())
        }
        scanNInts(0, n, Nil)
      }

      def scanNLists(nLists : Int, nElems : Int) : List[List[Int]] = {
        val seqLs = for (i <- 0 until nLists) yield scanNInts(nElems)
        seqLs.toList
      }

      try {
        val nKnaps = sc.nextInt()
        val nObjs = sc.nextInt()
        val objWeights = scanNInts(nObjs)
        val knapCapacities = scanNInts(nKnaps)
        val constraints = scanNLists(nKnaps, nObjs)
        Some(MultiKnapsackProblem(nKnaps, nObjs, objWeights, knapCapacities, constraints))
      }
      catch {
        case e : Exception =>
          println("Error while loading problem : " + e.getMessage())
          None
      }
    }

    val scan = new Scanner(new File(strFile))
    val mkProb = loadMKProblem(scan)
    scan.close()
    mkProb
  }

  /**
   * Establish the initial probability for each object to be in the knapsack
   * That probability is 0.5
   * @param nObjs The number of objects
   * @return a list of nObjs values of 0.5
   */
  def initialProbabilities(nObjs : Int) : List[Float] = {
    def initialProbabilities(i : Int, lsAcc : List[Float]) : List[Float] = {
      if (i == nObjs)
        lsAcc
      else
        initialProbabilities(i+1, 0.8F :: lsAcc)
    }

    initialProbabilities(0, Nil)
  }

  /**
   * Generates a random sample multiknapsack assignment
   * @param probVector list of probability parameters for an object to be
   *                   selected in the knapsack
   * @return the random knapsack assignment according to the probability
   *         distribution
   */
  def generateSample(probVector : List[Float]) : List[Int] = {
    def generateSample(probVector : List[Float], lsAcc : List[Int]) : List[Int] = probVector match {
      case Nil => lsAcc
      case p :: ps =>
        val rdVar = new IntRandomVar("IRdV", new BinomialDistribution(2, p))
        val rdVal = rdVar.getValue
        generateSample(ps, lsAcc :+ rdVal)
    }

    generateSample(probVector, Nil)
  }

  /**
   * Generates nSamples of cut vectors according to the binomial distribution
   * parameters given by probVector
   * @param probVector list of probability parameters for a node
   *                   to be in the same partition as node 1
   * @return a list of tuples (vector cut, cost of vector cut)
   */
  def generateSamplesWithS(mkProb : MultiKnapsackProblem, probVector : List[Float]) : List[(List[Int], Int)] = {
    def generateSamplesWithS(probVector : List[Float],
                             i : Int,
                             lsAcc : List[(List[Int], Int)]) : List[(List[Int], Int)] = {
      def generateSampleWithS(probVector : List[Float],
                              lsAcc : List[Int]) : (List[Int], Int) = probVector match {
        case Nil => (lsAcc, mkProb.evaluateS(lsAcc))
        case p :: ps =>
          val rdVar = new IntRandomVar("IRdV", new BinomialDistribution(2, p))
          val rdVal = rdVar.getValue
          generateSampleWithS(ps, lsAcc :+ rdVal)
      }

      if (i == nSamples)
        lsAcc
      else {
        generateSamplesWithS(probVector, i+1, generateSampleWithS(probVector, Nil)::lsAcc)
      }
    }

    generateSamplesWithS(probVector, 0, Nil)
  }

  /**
   * Compute the rho-quantile of the random samples
   * @param lsS a list of tuples (sample, performance), sorted by performance
   * @return the (1-rho)*nNodes element of lsS
   */
  def rhoQuantile(lsS : List[(List[Int], Int)]) : (List[Int], Int) = {
    val qtile = Math.ceil((1-rho)*nSamples).toInt

    if (qtile > lsS.length)
      lsS(lsS.length - 1)
    else if (qtile == 0)
      lsS(qtile)
    else
      lsS(qtile-1)
  }

  /**
   * Compute the new probability parameters
   *
   * @param sLs a sorted list of tuples "cut vector - performance"
   * @param perfQtile performance of the rho-quantile sample
   * @return the list of updated probability parameters
   */
  def computeNewProbabilities(nObjs : Int, sLs : List[(List[Int], Int)], perfQtile : Float) : List[Float] = {
    def computeProbAcc(i : Int, j : Int, accSumPonderate : Int, accSumHigherPerfs : Int) : Float = {
      if (i == sLs.length)
        accSumPonderate.toFloat / accSumHigherPerfs.toFloat
      else {
        val sLs_i = sLs(i)
        val accPonderate = if ((sLs_i._2 >= perfQtile) && (sLs_i._1(j) == 1)) 1 else 0
        val accHigherPerf = if (sLs_i._2 >= perfQtile) 1 else 0
        computeProbAcc(i+1, j, accSumPonderate+accPonderate, accSumHigherPerfs+accHigherPerf)
      }
    }

    val seqProb = for (j <- 0 until nObjs) yield computeProbAcc(0, j, 0, 0)
    seqProb.toList
  }

  /**
   * Stop criterion for the CE loop : probability parameters are all 0 or 1
   * @param ls a list of float numbers
   * @return true iff each element of ls is 0 or 1
   */
  def isDegenerateBinaryList(ls : List[Float]) : Boolean = ls match {
    case Nil => true
    case p :: ps =>
      if ((p == 1F) || (p == 0F))
        isDegenerateBinaryList(ps)
      else
        false
  }

  def stopCriterion(ls : List[Float]) : Boolean = {
    def stopCriterion(ls : List[Float], acc : Float) : Boolean = ls match {
      case Nil => acc <= threshold
      case x :: xs => stopCriterion(xs, Math.max(acc, Math.min(x, 1-x)))
    }

    ls match {
      case Nil => false
      case x :: xs => stopCriterion(xs, Math.min(x, 1-x))
    }
  }

  /**
   * Main Cross-Entropy loop, with a first iteration already done
   *
   * @param probList the list of probabilities for an object to be selected
   *                 in knapsack
   * @param bestFound the candidate assignment found in last iteration
   * @return the optimum assignment with its value
   */
  def crossEntropyLoop(mkProb : MultiKnapsackProblem,
                       probList : List[Float],
                       bestFound : (List[Int], Int)) : (List[Int], Int) = {
    if (stopCriterion(probList))
      bestFound
    else {
      val sortedLs = generateSamplesWithS(mkProb, probList).sortBy(tup => tup._2)
      val posQtile = rhoQuantile(sortedLs)
      val bestSample = sortedLs(nSamples-1)
      val newProbList = computeNewProbabilities(probList.length, sortedLs, posQtile._2)
      /////////// Debug info ///////////
      i += 1
      println(s"Iteration : $i")
      println("----------")
      println(s"Arr Quantile = $posQtile")
      println("----------")
      println(s"Best in samples = $bestSample")
      println("----------")
      println(s"New Prob List = $newProbList")
      println("----------")
      //////////////////////////////////
      val newBestFound = if (bestSample._2 > bestFound._2) bestSample else bestFound
      if (newBestFound._2 == posQtile._2)
        newBestFound
      else
        crossEntropyLoop(mkProb, newProbList, newBestFound)
    }
  }

  def main(args: Array[String]) {
    /*
    val nKnapsacks = 2
    val nObjects = 28
    val p0 = initialProbabilities(nObjects)
    val ls = generateSample(p0)
    val sls = mkProblem.evaluateS(ls)
    println("First Generated sample : "  + ls)
    println("Evaluation of S = " + mkProblem.evaluateS(ls))
    val bestKS = crossEntropyLoop(p0, (ls, sls))
    println("Best sample : "  + bestKS)
    */
    val optProb = loadMKProblem("/tmp/MultiKS.txt")
    optProb match {
      case Some(mkP) =>
        val p0 = initialProbabilities(mkP.nObjects)
        val ls = generateSample(p0)
        val sls = mkP.evaluateS(ls)
        println("First Generated sample : "  + ls)
        println("Evaluation of S = " + mkP.evaluateS(ls))
        val bestKS = crossEntropyLoop(mkP, p0, (ls, sls))
        println("Best sample : "  + bestKS)
      case None =>
        println("Error while loading problem")
    }
    //val ls = List(0,0,1,0,1,0,0,1,0,1,0,0,0,1,0,0,0,0,0,0,1,0,1,1,0,0,0,1)
    //val ls = List(0,0,1,0,1,1,1,1,0,1,0,1,0,1,1,0,0,0,0,0,1,0,1,1,0,1,0,0)
    //val ls = List(0,0,1,0,1,1,1,1,0,1,0,1,0,1,0,0,1,0,1,0,1,0,1,1,0,1,0,0)
    //println("Evaluation of S = " + mkProblem.evaluateS(ls))
  }
}