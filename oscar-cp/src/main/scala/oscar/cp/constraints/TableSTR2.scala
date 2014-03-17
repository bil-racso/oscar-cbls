package oscar.cp.constraints

import oscar.cp.core.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.core.CPPropagStrength
import oscar.algo.reversible.ReversibleInt
import scala.collection.mutable.HashSet
import oscar.algo.reversible.ReversibleBool

class TableSTR2(val X: Array[CPIntVar], table: Array[Array[Int]]) extends Constraint(X(0).store, "TableSTR"){
  
  val position = Array.tabulate(table.length)(i=>i)
  val currentLimit = new ReversibleInt(s,table.length-1)
  
  val arity = X.length
  val variablesIndexes = 0 until X.length
  val isBoundAndChecked = Array.fill(arity)(new ReversibleBool(s,false))
  val notGACValues = Array.fill(arity)(HashSet[Int]())
  
  val lastSize = Array.tabulate(X.length)(i=>new ReversibleInt(s,-1)) //last size must be initially different from the domain size
  
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
	val isInS_Sup = Array.fill(unboundVariableIndexes.length)(true) //true if there exist at least one value in the domain for which no support has been found yet
	val isInS_Val = Array.tabulate(unboundVariableIndexes.length){i =>
	  val inS_SVal = lastSize(i).value != X(i).size
	  lastSize(i).value = X(i).size
	  inS_SVal
	} //true if the domain of the variable changed during last execution of TableSTR2
	
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
	    if(isInS_Val(unboundCpVarIndex) && !X(unboundVariableIndexes(unboundCpVarIndex)).hasValue(tau(unboundVariableIndexes(unboundCpVarIndex)))) // check only variables in S_Val
	        isCurrentTupleValid = false
	    unboundCpVarIndex += 1      
	  }
	  
	  if(isCurrentTupleValid) {
	    unboundCpVarIndex = 0
	    while(unboundCpVarIndex < unboundVariableIndexes.length) {
	    	if(isInS_Sup(unboundCpVarIndex)) { 
	    	  notGACValues(unboundVariableIndexes(unboundCpVarIndex)).remove(tau(unboundVariableIndexes(unboundCpVarIndex)))
	    	  if(notGACValues(unboundVariableIndexes(unboundCpVarIndex)).isEmpty) isInS_Sup(unboundCpVarIndex) = false
	    	}
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
	  if(isInS_Sup(unboundCpVarIndex)) {
	    if (notGACValues(unboundVariableIndexes(unboundCpVarIndex)).size == X(unboundVariableIndexes(unboundCpVarIndex)).size)
	      return Failure
	      if(!notGACValues(unboundVariableIndexes(unboundCpVarIndex)).isEmpty) {
	        for(value <- notGACValues(unboundVariableIndexes(unboundCpVarIndex)))
	          X(unboundVariableIndexes(unboundCpVarIndex)).removeValue(value)
	      }
	    if(X(unboundVariableIndexes(unboundCpVarIndex)).isBound)
	      isBoundAndChecked(unboundVariableIndexes(unboundCpVarIndex)).setValue(true)
	    lastSize(unboundCpVarIndex).setValue(X(unboundVariableIndexes(unboundCpVarIndex)).size)
	  }
	  unboundCpVarIndex += 1
    }
	 
    Suspend
  }

}