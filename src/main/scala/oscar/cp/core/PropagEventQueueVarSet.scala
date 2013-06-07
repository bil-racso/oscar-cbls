package oscar.cp.core

/**
 * Trailable Queue of AC5 events
 * Each entry of the queue stores:
 *  - a delta
 *  - a index
 *  - a variable
 *  @author Pierre Schaus pschaus@gmail.com
 */
class PropagEventQueueVarSet(val next: PropagEventQueueVarSet, val cons: Constraint, val x: CPVarSet, val idx: Int) {
	
    def this(next: PropagEventQueueVarSet, cons: Constraint, x: CPVarSet) = {
      this(next,cons,x,0)
    }
    
    def hasNext() = next != null

	override def toString(): String = "PropagEventQueueVarSet constraint:"+cons+" var:"+x+" idx:"+idx;
	
	
	def size() = {
		var s = 0;
		var q = this;
		while (q != null) {
			s += 1
			q = q.next
		}
		s
	}

}
