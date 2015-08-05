package oscar.cp.scheduling.constraints

import oscar.cp.core._
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.CPOutcome._
import oscar.algo.SortUtils._
import scala.math.{min, max}
import scala.annotation.tailrec
import oscar.cp.scheduling.util.OpenSparseSet

/**
 * @author Steven Gay steven.gay@uclouvain.be
 */
/*
 *  Unary Detectable Precedences, with optional tasks and non-constant durations.
 *  As in "Extension of O(n log n) Filtering...", Vilim et al. Constraints 2005. 
 */

class UnaryEF(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], resources: Array[CPIntVar], id: Int)(implicit store: CPStore)
extends Constraint(store, "UnaryDetectablePrecedences") {
  val lr = new UnaryEFLR(starts, durations, ends, resources, id) 
  val rl = new UnaryEFLR(ends map(-_), durations, starts map(-_), resources, id)
  
  override def setup(strength: CPPropagStrength) = {
    try {
      if (store.add(Array(lr, rl)) == Failure) Failure
//      if (store.add(Array(lr)) == Failure) Failure
      else Suspend
    }
    catch {
      case e: NoSolutionException => Failure
      case e: Inconsistency => Failure
    }
  }
}




class UnaryEFLR(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], resources: Array[CPIntVar], id: Int)(implicit store: CPStore)
//extends Constraint(store, "UnaryEFLR")
extends UnaryTemplate(starts, durations, ends, resources, id, "UnaryDetectablePrecedencesLR")(store)
{
  private[this] val nTasks = starts.length
  
  priorityL2 = 2
  
  private def nextPowerOfTwo(k: Int): Int = {
    1 << math.ceil(math.log(k) / math.log(2)).toInt
  }
  
  // TODO: optimize the size of tree
  private[this] val workload    = Array.ofDim[Int](2 * nextPowerOfTwo(nTasks)) 
  private[this] val envelope    = Array.ofDim[Int](2 * nextPowerOfTwo(nTasks))   // base envelope level
  private[this] val workloadOpt = Array.ofDim[Int](2 * nextPowerOfTwo(nTasks))   // maximum workload of this node using at most one optional
  private[this] val envelopeOpt = Array.ofDim[Int](2 * nextPowerOfTwo(nTasks))   // maximum envelope of an optional activity at this node

  private[this] val leafOfActivity  = Array.ofDim[Int](nTasks)  // activity -> leaf id
  
  // for mergeSort
  private[this] val temp1 = Array.ofDim[Int](nTasks + 1)
  private[this] val temp2 = Array.ofDim[Int](nTasks + 1)

  private[this] val sortedBySMin = Array.tabulate(nTasks){ i => i }
  private[this] val sortedByEMax = Array.tabulate(nTasks){ i => i }
//  private[this] val sortedByEMin = Array.tabulate(nTasks){ i => i }
//  private[this] val sortedBySMax = Array.tabulate(nTasks){ i => i }
  
  private[this] val toConsiderBySMin = Array.ofDim[Int](nTasks)
  private[this] val toConsiderByEMax = Array.ofDim[Int](nTasks)
//  private[this] val toConsiderByEMin = Array.ofDim[Int](nTasks)
//  private[this] val toConsiderBySMax = Array.ofDim[Int](nTasks)
  
  
  override def propagate(): CPOutcome = {
    updateCache()
    
    
    // Step 1: Initialization
    // Step 1.1: sort activities by criteria of interest
    // TODO: with variable durations, we may be more precise by adding the energy of i, dmin(i),
    //       not at smin(i), but at emin(i) - dmin(i)
    
    filterSort(sortedBySMin, toConsiderBySMin, smin)
    filterSort(sortedByEMax, toConsiderByEMax, emax)
//    filterSort(sortedByEMin, toConsiderByEMin, emin)
//    filterSort(sortedBySMax, toConsiderBySMax, smax)

    // Step 1.2: initialize arrays of the tree
    /*
     *  Tree uses indices from 0 to 2 * nodes - 1
     *  Leaves go from nodes to 2 * nodes - 1
     *  Leaves are not all actually used, only those from nodes to nodes + nToConsider - 1  
     */
    val nToConsider = toConsider.limit.value
    val nodes = nextPowerOfTwo(nToConsider)
    
    // if (debug) println(s"UnaryEF: considering $nToConsider over $nTasks tasks, making a tree of ${2*nodes} nodes.")
    // if (nodes < nToConsider) throw new IllegalArgumentException()
    
    /*
     *  Initialize nodes: EF begins with \Theta = T and \Lambda = \emptyset
     */ 
    
    // indices in [nodes, nodes + nToConsider[ are initialized by their activity
    // make a map from activities to leaves while we're at it
    var p = nToConsider
    while (p > 0) {
      p -= 1
      val a = toConsiderBySMin(p)
      leafOfActivity(a) = nodes + p
      val l = leafOfActivity(a)
      
      if (required(a)) {
        workload(l) = dmin(a)
        envelope(l) = smin(a) + dmin(a)
      }
      else {
        workload(l) = 0
        envelope(l) = Int.MinValue
      }
      workloadOpt(l) = dmin(a)
      envelopeOpt(l) = smin(a) + dmin(a)      
    }

    // indices in [nodes + nToConsider, 2 * nodes[ are empty leaves
    p =  2 * nodes
    val pinf = nToConsider + nodes
    while (p > pinf) {
      p -= 1
      workload(p) = 0
      envelope(p) = Int.MinValue
      workloadOpt(p) = 0
      envelopeOpt(p) = Int.MinValue      
    }

    // indices in [1, nodes[ are internal nodes that we initialize by their children
    p = nodes
    while (p > 1) {
      p -= 1
      val left = p << 1
      val right = 1 + left
      workload(p) = workload(left) + workload(right)
      envelope(p) = max(envelope(left) + workload(right), envelope(right))
      
      workloadOpt(p) = max(workloadOpt(left) + workload(right),
                           workload(left) + workloadOpt(right))
                           
      envelopeOpt(p) = max(envelopeOpt(right),
                         max(envelope(left) + workloadOpt(right),
                             envelopeOpt(left) + workload(right))
                        )
    }
    
    
    // Step 2: Do main loop
    p = nToConsider
    var q = 0
    
    while (p > 0) {
      p -= 1
      
      val j = toConsiderByEMax(p)
      
      if (required(j))  if (envelope(1) > emax(j)) throw Inconsistency  // overload
      
      
      // remove optional with overload checking
      while (envelopeOpt(1) > emax(j)) {
        val opt = getMaxOptional(1, nodes)  // get responsible optional leaf
        val i = toConsiderBySMin(opt-nodes)  // get activity of that leaf
        
        if (required(i)) { // apply Edge Finding
          if (envelope(1) > smin(i) && starts(i).updateMin(envelope(1)) == Failure) throw Inconsistency
          removeFromLambda(i)
        }
        else {  // remove responsible, using Overload Checking
          if (resources(i).removeValue(id) == Failure) throw Inconsistency
          toConsider.exclude(i)
          removeFromLambda(i)
        }
      }
      
      // removeFromTheta(j)
      // if (required(j)) addToLambda(j)
      
      
      if (required(j)) {
        // move from theta to lambda
        val l = leafOfActivity(j)
        workload(l) = 0
        envelope(l) = Int.MinValue
        // workloadOpt(l) = emin(j) - smin(j)
        // envelopeOpt(l) = emin(j)
        insertActivity(l/2)
      }
      else {
        removeFromLambda(j)
      }
      
    }
      
    // removeExtremal()
    removeIsolated(toConsiderBySMin, toConsiderByEMax, nToConsider)
    Suspend
  }
  
  @inline final def addToTheta(act: Int) = {
    val leaf = leafOfActivity(act)
    workload(leaf)    = dmin(act)
    envelope(leaf)    = smin(act) + dmin(act)
    workloadOpt(leaf) = dmin(act)
    envelopeOpt(leaf) = smin(act) + dmin(act)
    
    // if (debug) println(s"envelope ${envelope(leaf)}")
    
    insertActivity(leaf / 2)
  }

  @inline final def addToLambda(act: Int) = {
    val leaf = leafOfActivity(act)
    // workload(leaf)    = 0
    // envelope(leaf)    = Int.MinValue
    workloadOpt(leaf) = dmin(act)
    envelopeOpt(leaf) = smin(act) + dmin(act)
    
    insertActivity(leaf / 2)
  }
  
  @inline final def removeFromTheta(act: Int) = {
    val leaf = leafOfActivity(act)
    workload(leaf)    = 0
    envelope(leaf)    = Int.MinValue
    workloadOpt(leaf) = 0
    envelopeOpt(leaf) = Int.MinValue
    
    insertActivity(leaf / 2)
  }

  @inline final def removeFromLambda(act: Int) = {
    val leaf = leafOfActivity(act)
    // workload(leaf)    = 0
    // envelope(leaf)    = Int.MinValue
    workloadOpt(leaf) = 0
    envelopeOpt(leaf) = Int.MinValue
    
    insertActivity(leaf / 2)
  }
  

  @tailrec
  final def insertActivity(t: Int): Unit = {
    if (t > 0) {
      val left = t << 1
      val right = 1 + left
        
      workload(t) = workload(left) + workload(right)
      envelope(t) = max(envelope(left) + workload(right), envelope(right))
      
      workloadOpt(t) = max(workloadOpt(left) + workload(right),
                           workload(left) + workloadOpt(right))
                           
      envelopeOpt(t) = max(envelopeOpt(right),
                         max(envelope(left) + workloadOpt(right),
                             envelopeOpt(left) + workload(right))
                        )
      
      // if (debug) println(s"envelope ${envelope(t)}, left = ${envelope(left)}, workload = ${workload(right)}, right = ${envelope(right)}")
      insertActivity(t/2)
    } 
  }
  
  /*
   * find which optional activity causes the value of envelopeOpt
   * This method is more lengthy than remembering the activity at each node,
   * but when activity removal is rare, recomputing on need is cheaper.
   */
  @tailrec
  final def getMaxOptional(t:Int, tMax: Int): Int = {
    if (t >= tMax) t  // reached a leaf
    else {
      val left = t << 1
      val right = 1 + left
      val e = envelopeOpt(t) 
      
      if (e == envelopeOpt(right)) {
        getMaxOptional(right, tMax)
      }
      else if (e == envelopeOpt(left) + workload(right)) {
        getMaxOptional(left, tMax)
      }
      else {
        getMaxOptionalWorkload(right, tMax)
      }
    }
  }
  
  
  /*
   * find which optional activity causes the value of workloadOpt
   */
  @tailrec
  final def getMaxOptionalWorkload(t: Int, tMax: Int): Int = {
    if (t >= tMax) t  // reached a leaf
    else {
      val left = t << 1
      val right = 1 + left
      val w = workloadOpt(t)
      
      if (w == workloadOpt(left) + workload(right)) {
        getMaxOptionalWorkload(left, tMax)
      }
      else {
        assert(w == workload(left) + workloadOpt(right))
        getMaxOptionalWorkload(right, tMax)
      }
        
    }
  }
  
  private[this] val indices = new Array[Int](nTasks)
  // filters out tasks that should not be considered, then sorts them
  final def filterSort(byKey: Array[Int], filtered: Array[Int], keys: Array[Int]): Unit = {
    val limit = toConsider.limit.value
    val status = toConsider.status
    
    var p = byKey.length 
    var q = limit
    
    // extract only values to consider
    while (p > 0) {
      p -= 1
      val task = byKey(p)
      if (status(task) < limit) {
        q -= 1
        filtered(q) = task
        indices(q) = p
      }
    }
    
    // sort them
    mergeSort(filtered, keys, 0, limit, temp1, temp2)
    
    // put them back for mergeSort's incremental behaviour
    p = limit
    while (p > 0) {
      p -= 1
      byKey(indices(p)) = filtered(p) 
    }
  }

}

object UnaryEF {
  def apply(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], resources: Array[CPIntVar], id: Int)(implicit store: CPStore) = 
    new UnaryEF(starts, durations, ends, resources, id) 

}