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


package oscar.cp.constraints

import oscar.algo.reversible._
import oscar.cp.core.CPOutcome
import oscar.cp.modeling._
import scala.collection.JavaConversions._
import oscar.cp.core._
import scala.annotation.tailrec
import scala.util.control.Breaks._



/**
 * This code is adapted to CP from an original implementation by Kevin L. Stern of the Hungarian algorithm
 * @author Pierre Schaus pschaus@gmail.com
 */
class MinAssignment(val xarg: Array[CPIntVar], val weightsarg: Array[Array[Int]], val cost: CPIntVar) extends Constraint(xarg(0).s, "MinAssignment") {
  if (weightsarg.size != xarg.size) throw new IllegalArgumentException("MinAssignment: dim of x and weights must match")
  val n = weightsarg(0).size
  val x = xarg ++ Array.fill(n-xarg.size)(CPIntVar(0 until n)(s)) 
  val weights = weightsarg ++ Array.fill(n-xarg.size)(Array.fill(n)(0))
  val Jmax = n
  val Wmax = n
  val J = 0 until n
  val W = 0 until n
  val M = (weights.flatten.max + 1) * n

  val costMatrix = Array.tabulate(n, n)((i, j) => new ReversibleInt(s, if (!x(i).hasValue(j)) M else weights(i)(j)))

  val labelByWorker = Array.fill(n)(new ReversibleInt(s, 0))
  val labelByJob = Array.fill(n)(new ReversibleInt(s, 0))
  // for each worker, the job it is assigned to and for each job, the worker it is assigned to
  val matchJobByWorker = Array.fill(n)(new ReversibleInt(s, -1))
  val matchWorkerByJob = Array.fill(n)(new ReversibleInt(s, -1))

  
  // minSlackValueByJob[j] = argmin(costMatrix[w][j] - labelByWorker[w] - labelByJob[j]) | w is unmatched)  
  val minSlackWorkerByJob = Array.fill(n)(0)
  // minSlackValueByJob[j] = min(costMatrix[w][j] - labelByWorker[w] - labelByJob[j]) | w is unmatched)
  val minSlackValueByJob = Array.fill(n)(0)
  // used by the algo in executionPhase
  val parentWorkerByCommittedJob = Array.fill(n)(0)
  val committedWorkers = Array.fill(n)(false)

  def printAll() {
    println("-------min assignment-----")
    println("M:" + M)
    println("costMatrix:")
    for (w <- W) println(costMatrix(w).mkString(","))
    println("labelByWorker:")
    println(labelByWorker.mkString(","))
    println("labelByJob:")
    println(labelByJob.mkString(","))
    println("matchJobByWorker:")
    println(matchJobByWorker.mkString(","))
    println("matchWorkerByJob:")
    println(matchJobByWorker.mkString(","))
    println("minSlackWorkerByJob:")
    println(minSlackWorkerByJob.mkString(","))
    println("minSlackValueByJob:")
    println(minSlackValueByJob.mkString(","))
    println("parentWorkerByComittedJob:")
    println(parentWorkerByCommittedJob.mkString(","))
  }

  /**
   * Helper method to record a matching between worker w and job j.
   */
  def assign(w: Int, j: Int) {
    matchJobByWorker(w).value = j;
    matchWorkerByJob(j).value = w;
  }

  /**
   * Compute an initial feasible solution by assigning zero labels to the
   * workers and by assigning to each job a label equal to the minimum cost
   * among its incident edges.
   */
  private def computeInitialFeasibleSolution() {
    for (j <- J) {
      labelByJob(j).value = W.map(costMatrix(_)(j).value).min
    }
  }

  /**
   * Initialize the next phase of the algorithm by clearing the committed
   * workers and jobs sets and by initializing the slack arrays to the values
   * corresponding to the specified root worker.
   *
   * @param w the worker at which to root the next phase.
   */
  private def initializePhase(w: Int) {
    java.util.Arrays.fill(committedWorkers, false)
    java.util.Arrays.fill(parentWorkerByCommittedJob, -1)
    committedWorkers(w) = true
    for (j <- J) {
      minSlackValueByJob(j) = slack(w, j)
      minSlackWorkerByJob(j) = w;
    }
  }

  private def reduce() {
    for (w <- W) {
      val min = costMatrix(w).map(_.value).min
      for (j <- J) costMatrix(w)(j).value = costMatrix(w)(j).value - min
    }
    for (j <- J) {
      val min = W.map(w => costMatrix(w)(j).value).min
      for (w <- W) costMatrix(w)(j).value = costMatrix(w)(j).value - min
    }
  }

  private def fetchUnmatchedWorker() = W.find(w => matchJobByWorker(w).value == -1).getOrElse(n)

  /**
   * Execute a single phase of the algorithm. A phase of the Hungarian
   * algorithm consists of building a set of committed workers and a set of
   * committed jobs from a root unmatched worker by following alternating
   * unmatched/matched zero-slack edges. If an unmatched job is encountered,
   * then an augmenting path has been found and the matching is grown. If the
   * connected zero-slack edges have been exhausted, the labels of committed
   * workers are increased by the minimum slack among committed workers and
   * non-committed jobs to create more zero-slack edges (the labels of
   * committed jobs are simultaneously decreased by the same amount in order
   * to maintain a feasible labeling).
   * <p>
   *
   * The runtime of a single phase of the algorithm is O(n^2), where n is the
   * dimension of the internal square cost matrix, since each edge is visited
   * at most once and since increasing the labeling is accomplished in time
   * O(n) by maintaining the minimum slack values among non-committed jobs.
   * When a phase completes, the matching will have increased in size.
   */
  private def executePhase() {
    while (true) {
      var minSlackWorker = -1
      var minSlackJob = -1;
      var minSlackValue = Int.MaxValue;
      for (j <- J) {
        if (parentWorkerByCommittedJob(j) == -1) {
          if (minSlackValueByJob(j) < minSlackValue) {
            minSlackValue = minSlackValueByJob(j);
            minSlackWorker = minSlackWorkerByJob(j);
            minSlackJob = j;
          }
        }
      }
      if (minSlackValue > 0) {
        updateLabeling(minSlackValue);
      }
      parentWorkerByCommittedJob(minSlackJob) = minSlackWorker;
      if (matchWorkerByJob(minSlackJob).value == -1) {
        // An augmenting path has been found		 
        var committedJob = minSlackJob;
        var parentWorker = parentWorkerByCommittedJob(committedJob)
        while (true) {
          var temp = matchJobByWorker(parentWorker).value
          assign(parentWorker, committedJob);
          committedJob = temp;
          if (committedJob == -1) {
            return
          }
          parentWorker = parentWorkerByCommittedJob(committedJob)
        }
      } else {
        // Update slack values since we increased the size of the committed workers set.		 
        var worker: Int = matchWorkerByJob(minSlackJob).value
        committedWorkers(worker) = true
        for (j <- J) {
          if (parentWorkerByCommittedJob(j) == -1) {
            val s = slack(worker, j)
            if (minSlackValueByJob(j) > s) {
              minSlackValueByJob(j) = s
              minSlackWorkerByJob(j) = worker
            }
          }
        }
      }
    }
  }

  /**
   * Update labels with the specified slack by adding the slack value for
   * committed workers and by subtracting the slack value for committed jobs.
   * In addition, update the minimum slack values appropriately.
   */
  def updateLabeling(slack: Int) {
    for (w <- W; if committedWorkers(w)) {
      labelByWorker(w).value = labelByWorker(w).value + slack;
    }
    for (j <- J) {
      if (parentWorkerByCommittedJob(j) != -1) {
        labelByJob(j).value = labelByJob(j).value - slack;
      } else {
        minSlackValueByJob(j) -= slack;
      }
    }
  }

  private def findMinAssignment() {
    // Heuristics to improve performance: Reduce rows and columns by their
    // smallest element, compute an initial non-zero dual feasible solution
    // and create a greedy matching from workers to jobs of the cost matrix.
    greedyMatch();
    var w = fetchUnmatchedWorker()
    //println("fetching worker:" + w)
    while (w < n) {
      //println("initialize phase")
      initializePhase(w)
      executePhase()
      //printAll()
      w = fetchUnmatchedWorker()
      //println("fetching worker:" + w)
    }
    //println("end of execute")
    //printAll()
    
  }

  def slack(w: Int, j: Int) = costMatrix(w)(j).value - labelByWorker(w).value - labelByJob(j).value

  /**
   * Find a valid matching by greedily selecting among zero-cost matchings.
   * This is a heuristic to jump-start the augmentation algorithm.
   */
  private def greedyMatch() {
    var w = 0
    var j = 0
    while (w < Wmax) {
      j = 0
      while (j < Jmax) {
        if (matchJobByWorker(w).value == -1 && matchWorkerByJob(j).value == -1 && slack(w, j) == 0)
        assign(w, j);
        j += 1
      }
      w += 1
    }
    
    
    /*
    for (w <- W; j <- J)
      if (matchJobByWorker(w).value == -1 && matchWorkerByJob(j).value == -1 && slack(w, j) == 0)
        assign(w, j);
       
    */
  }

  private def filter(): CPOutcome = {

    //printAll()
    //val sum = (W).foldLeft(0)((tot, w) => tot + weights(w)(matchJobByWorker(w).value))
    var sum = 0
    var w = 0
    while (w < Wmax) {
      sum += weights(w)(matchJobByWorker(w).value)
      w += 1
    }
    
    
    if (cost.updateMin(sum) == CPOutcome.Failure) return CPOutcome.Failure
    val maxSlack = cost.max - sum
    
    /*
    for (w <- W; j <- J; if x(w).hasValue(j)) {
    	if (slack(w,j) > maxSlack) {
    	  if (x(w).removeValue(j) == CPOutcome.Failure) {
    	    return CPOutcome.Failure
    	  }
    	}
    }
    */
    w = 0
    var j = 0
    while (w < Wmax) {
      j = 0
      while (j < Jmax) {
    	if (slack(w,j) > maxSlack) {
    	  if (x(w).removeValue(j) == CPOutcome.Failure) {
    	    return CPOutcome.Failure
    	  }
    	}        
        j += 1
      }
      w += 1
    }
    CPOutcome.Suspend
  }
  


  override def setup(l: CPPropagStrength): CPOutcome = {
    if (s.post(new AllDifferent(x:_*),l) == CPOutcome.Failure) {
      return CPOutcome.Failure;
    }
    reduce()
    computeInitialFeasibleSolution();
    findMinAssignment()
    if (filter() == CPOutcome.Failure) return CPOutcome.Failure
    for (i <- 0 until n; if !x(i).isBound) {
      x(i).callValRemoveIdxWhenValueIsRemoved(this, i)
      x(i).callPropagateWhenDomainChanges(this)
    }
    if (!cost.isBound) {
      cost.callPropagateWhenMaxChanges(this)
    }
    CPOutcome.Suspend
  }

  override def valRemoveIdx(y: CPIntVar, w: Int, j: Int): CPOutcome = {
    costMatrix(w)(j).value = M
    if (matchJobByWorker(w).value == j) {
      matchJobByWorker(w).value = -1
      matchWorkerByJob(j).value = -1
    }
    CPOutcome.Suspend
  }

  override def propagate(): CPOutcome = {
    val valid = W.forall(w => matchJobByWorker(w).value != -1)
    if (!valid) {
      findMinAssignment()
    }
    filter()
  }

}

object MinAssignment {
  def main(args: Array[String]) {

  }
}
