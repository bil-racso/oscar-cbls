package oscar.cp.constraints

import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.core.CPPropagStrength
import oscar.algo.reversible.ReversibleInt
import scala.collection.mutable.HashSet
import oscar.algo.reversible.ReversibleBoolean

class TableSTR(val X: Array[CPIntVar], table: Array[Array[Int]]) extends Constraint(X(0).store, "TableSTR"){
  
  val position = Array.tabulate(table.length)(i=>i)
  val currentLimit = new ReversibleInt(s,table.length-1)
  
  val arity = X.length
  val variablesIndexes = 0 until X.length
  val isBoundAndChecked = Array.fill(arity)(new ReversibleBoolean(s,false))
  val notGACValues = Array.fill(arity)(HashSet[Int]())
  
  override def setup(l: CPPropagStrength): CPOutcome = {
    idempotent = true
    if (propagate() == CPOutcome.Failure) return CPOutcome.Failure
    X.filter(!_.isBound).foreach(_.callPropagateWhenDomainChanges(this))
    Suspend
  }
  
  override def propagate(): CPOutcome = {
	
    var i = 0
    while(i < arity) {
      notGACValues(i).clear 
      notGACValues(i) ++= X(i)
      i += 1
    }
			
	val unboundVariableIndexes = variablesIndexes.filter(i => !isBoundAndChecked(i).value)
	
	i = 0
	var unboundCpVarIndex = -1
	var index = -1
	var tau = Array[Int]()
	var isCurrentTupleValid = true
	var tmpPosition = -1
	
	while (i <= currentLimit.getValue) {
	  index = position(i)
	  tau = table(index)
	  
	  //if is validTuple
	  unboundCpVarIndex = 0
	  isCurrentTupleValid = true
	  while (unboundCpVarIndex < unboundVariableIndexes.length && isCurrentTupleValid) {
	    if(!X(unboundVariableIndexes(unboundCpVarIndex)).hasValue(tau(unboundVariableIndexes(unboundCpVarIndex))))
	        isCurrentTupleValid = false
	    unboundCpVarIndex += 1      
	  }
	  
	  if(isCurrentTupleValid) {
	    unboundCpVarIndex = 0
	    while(unboundCpVarIndex < unboundVariableIndexes.length) {
	    	notGACValues(unboundVariableIndexes(unboundCpVarIndex)).remove(tau(unboundVariableIndexes(unboundCpVarIndex)))
	    	unboundCpVarIndex += 1
	    }
	    i += 1
	  }
	  else { //removeTuple
	    tmpPosition = position(i)
	    position(i) = position(currentLimit.getValue)
	    position(currentLimit.getValue) = tmpPosition
	    currentLimit.setValue(currentLimit.getValue - 1)
	  }
	    
	}
	
	unboundCpVarIndex = 0
	while(unboundCpVarIndex < unboundVariableIndexes.length) {
	  if (notGACValues(unboundVariableIndexes(unboundCpVarIndex)).size == X(unboundVariableIndexes(unboundCpVarIndex)).size)
	    return Failure
	  if(!notGACValues(unboundVariableIndexes(unboundCpVarIndex)).isEmpty) {
	    for(value <- notGACValues(unboundVariableIndexes(unboundCpVarIndex)))
	      X(unboundVariableIndexes(unboundCpVarIndex)).removeValue(value)
	  }
	  if(X(unboundVariableIndexes(unboundCpVarIndex)).isBound)
	    isBoundAndChecked(unboundVariableIndexes(unboundCpVarIndex)).setValue(true)
	  unboundCpVarIndex += 1
    }
	 
    Suspend
  }

}