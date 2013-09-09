package oscar.util.mo

object ArchiveUtils {
  def getGlobalArchive(archives: Array[Set[MOOPoint[Double]]], comparator: MOOComparator[Double]): LinearList[Double] = {
    val globalArchive = LinearList[Double]()
    for (archive <- archives; point <- archive) {
      globalArchive.insert(point, comparator)
    }
    globalArchive
  }
  
  def comparePurity(names: Array[String], archives: Array[Set[MOOPoint[Double]]], comparator: MOOComparator[Double]): Array[(String, Double)] = {
    Array.tabulate(names.length)(i => (names(i), getPurity(archives(i), getGlobalArchive(archives, comparator))))
  }
  
  def getPurity(archive: Set[MOOPoint[Double]], globalArchive: ParetoFront[Double]): Double = {
    var sum = 0.0
    for (point <- archive) {
      if (globalArchive.contains(point)) sum += 1.0
    }
    sum / globalArchive.size
  }
  
  def compareDeltas(names: Array[String], archives: Array[Set[MOOPoint[Double]]], comparator: MOOComparator[Double]): Array[(String, Double)] = {
    val extremePoints = getExtremePoints(getGlobalArchive(archives, comparator).toSet)
    Array.tabulate(names.length)(i => (names(i), getDelta(archives(i), extremePoints)))
  }
  
  def getDelta(archive: Set[MOOPoint[Double]], extremePoints: Array[(Double, Double)]): Double = {
    var maxDelta = Double.MinValue
    val sortedEvaluations = getSortedEvaluations(archive)
    for (dim <- 0 until sortedEvaluations.length) {
      val delta0j = math.abs(extremePoints(dim)._1 - sortedEvaluations(dim)(0))
      val deltaNj = math.abs(extremePoints(dim)._2 - sortedEvaluations(dim)(sortedEvaluations.length - 1))
      val averageDistance = getAverageDistance(sortedEvaluations(dim))
      val denom = delta0j + deltaNj + (sortedEvaluations(dim).length - 1) * averageDistance
      var num = delta0j + deltaNj
      for (evalNum <- 0 until sortedEvaluations(dim).length - 1) {
        num += math.abs(math.abs(sortedEvaluations(dim)(evalNum + 1) - sortedEvaluations(dim)(evalNum)) - averageDistance)
      }
      maxDelta = math.max(maxDelta, num / denom)
    }
    maxDelta
  }
  
  def getSortedEvaluations(archive: Set[MOOPoint[Double]]): Array[Array[Double]] = {
    var sortedEvaluations = List[Array[Double]]()
    for (dim <- archive.head.nbEvaluations - 1 to 0 by -1) {
      sortedEvaluations ::= archive.map(point => point.getEvaluation(dim)).toArray.sortWith((e1, e2) => e1 < e2)
    }
    sortedEvaluations.toArray
  }
  
  def getAverageDistance(evaluationsAtAGivenDim: Array[Double]): Double = {
    var sum = 0.0
    for (i <- 0 until evaluationsAtAGivenDim.length - 1) {
      sum += math.abs(evaluationsAtAGivenDim(i + 1) - evaluationsAtAGivenDim(i))
    }
    sum / (evaluationsAtAGivenDim.length - 1)
  }
  
  def getExtremePoints(archive: Set[MOOPoint[Double]]): Array[(Double, Double)] = {
    val extremeValues = Array.fill(archive.head.nbEvaluations)((Double.MaxValue, Double.MinValue))
    for(point <- archive; dim <- 0 until archive.head.nbEvaluations) {
      if (point.getEvaluation(dim) < extremeValues(dim)._1) extremeValues(dim) = (point.getEvaluation(dim), extremeValues(dim)._2)
      if (point.getEvaluation(dim) > extremeValues(dim)._2) extremeValues(dim) = (extremeValues(dim)._1, point.getEvaluation(dim))
    }
    extremeValues
  }
  
  def getMostDistantConsecutivePoints[E](archive: ParetoFront[E]): Array[ArchiveElement[E]] = {
    val nbEvals = archive.head.nbEvaluations
    if (archive.size == 1) {
      return Array(archive.randomElement)
    }
    val orderedPointsByEvals = Array.tabulate(nbEvals)(i => {
      archive.getElements.sortWith((p1, p2) => p1.getEvaluation(i).asInstanceOf[Double] < p2.getEvaluation(i).asInstanceOf[Double])
    })
    val scalingFactors = Array.tabulate(nbEvals)(i => orderedPointsByEvals(i)(orderedPointsByEvals(i).length - 1).getEvaluation(i).asInstanceOf[Double] - orderedPointsByEvals(i)(0).getEvaluation(i).asInstanceOf[Double])
    val maxDistances = Array.tabulate(nbEvals)(i => {
      var maxDist = (orderedPointsByEvals(i)(0).getEvaluation(i).asInstanceOf[Double] - orderedPointsByEvals(i)(1).getEvaluation(i).asInstanceOf[Double], orderedPointsByEvals(i)(0), orderedPointsByEvals(i)(1))
      for (j <- 0 until orderedPointsByEvals(i).length - 1) {
        val newDist = orderedPointsByEvals(i)(j).getEvaluation(i).asInstanceOf[Double] - orderedPointsByEvals(i)(j + 1).getEvaluation(i).asInstanceOf[Double]
        if (newDist >= maxDist._1) {
          maxDist = (newDist, orderedPointsByEvals(i)(j), orderedPointsByEvals(i)(j + 1))
        }
      }
      maxDist
    })
    val overallMaxDist = maxDistances.foldLeft(maxDistances(0))((curMax, newDist) => if (newDist._1 >= curMax._1) newDist else curMax)
    Array(overallMaxDist._2, overallMaxDist._3)
  }
}
