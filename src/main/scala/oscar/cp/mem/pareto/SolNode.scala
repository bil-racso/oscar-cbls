package oscar.cp.mem.pareto

class SolNode[Sol](val sol: MOSol[Sol], val objsNode: Array[LinkedNode[SolNode[Sol]]]) { 
  override def toString = sol.toString 
}
