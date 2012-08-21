package oscar.cp.constraints

import scala.math.max
import scala.math.min

import oscar.cp.scheduling.CumulativeActivity

class MaxEnergyOverloadChecking {

	
	class ThetaTree(allTasks : Array[CumulativeActivity], taskOrder : Array[Int], capacity : Int) {
		
		private val tree = new Array[ThetaTreeNode](size(allTasks.size))
		
		private var tasks  : Array[CumulativeActivity] = null
		private var order  : Array[Int] = null
		private var nTasks : Int = 0
		private var size   : Int = 0
		private var capa   : Int = 0
		
		init(allTasks, order, capacity)

		def init(allTasks : Array[CumulativeActivity], order : Array[Int], capacity : Int) {
			
			tasks  = allTasks
			nTasks = tasks.size	
			size   = size(nTasks)
			capa   = capacity		
			
			var i = size-1
			while(i >= size - nTasks) {
				
				// Compute Leaf
				
				i -= 1
			}
			
			i = size - nTasks - 1
			while (i >= 0) {
				
				// Compute Node
				
				i -= 1
			}
		}
		
		def computeLeafVals(i : Int) {
			
			val t = order(nodeIdToTaskId(i))
			
			tree(i).e      = tasks(t).minEnergy
			tree(i).Env    = capa*tasks(t).est + tree(i).e
			tree(i).maxE   = Int.MaxValue
			tree(i).maxEnv = Int.MaxValue
		}
		
		def computeNodeVals(i : Int) {
			
			if (!isNode(left(i))) {
				
				clearNode(i)
				
			} else if (!isNode(right(i))) {
				
				tree(i) = tree(left(i))
				
			} else {
				
				tree(i).e      = tree(left(i)).e + tree(right(i)).e
				tree(i).Env    = max(tree(left(i)).Env + tree(right(i)).e, tree(right(i)).Env)
				tree(i).maxE   = Int.MaxValue
				tree(i).maxEnv = Int.MaxValue
			}		
		}
		
		def clearNode(i : Int) {
			
			tree(i).e      = 0
			tree(i).Env    = 0
			tree(i).maxE   = Int.MaxValue
			tree(i).maxEnv = Int.MaxValue
		}
		
		def add(j : Int) {
			
			var v = 0
			var midPoint = nTasks
			
			var leafNum = j - (size - nTasks)
			
			while (v < nTasks-1) {
				
				midPoint /= 2
				pushDown2(v)
				
				tree(v).maxE = Int.MaxValue
				tree(v).maxEnv = Int.MaxValue
				
				if (leafNum >= midPoint) {
					leafNum -= midPoint
					v = 2*v + 2
				} else {
					v = 2*v + 1
				}
			}
			
			assert(v == j)
			
			enableNode(j)
		}
		
		def pushDown2(v : Int) {
			
			val r = 2*v + 2
			val l = 2*v + 1
			
			tree(r).maxEnv = min(tree(v).maxEnv, tree(r).maxEnv)
			tree(l).maxEnv = min(tree(v).maxEnv - tree(r).e, tree(l).maxEnv)
			tree(r).maxE   = min(tree(v).maxEnv - tree(l).Env, min(tree(r).maxE - tree(r).maxE, tree(r).maxE))	
			tree(l).maxE   = min(tree(v).maxEnv - tree(r).e, tree(l).maxEnv)
		}
		
		def enableNode(i : Int) {
			computeLeafVals(i)
			updateTree(i)
		}
		
		def updateTree(j : Int) {
			
			var i = j
			while (i >= 0) {
				computeNodeVals(i)
				i = parent(i)
			}
		}
		
		
		def size(i : Int) = 2*i - 1
		
		def nodeIdToTaskId(i : Int) = i - (nTasks - 1)
		
		def isNode(i : Int) : Boolean = (i < size && tree(i) != null)
		
		def parent(i : Int)   = (i - 1)/2
		def left(i : Int)     = 2*i + 1
		def right(i : Int)    = 2*i + 2
		def siblingL(i : Int) = i - 1
		def siblingR(i : Int) = i + 1
	}
 	
	class ThetaTreeNode(var e : Int, var Env : Int, var maxE : Int, var maxEnv : Int) 
}