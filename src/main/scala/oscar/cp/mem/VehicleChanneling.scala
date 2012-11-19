package oscar.cp.mem

import oscar.cp.core._
import oscar.reversible.ReversibleSetIndexedArray

class VehicleChanneling(cp: Store, pred: Array[CPVarInt], succ: Array[CPVarInt], vehicle: Array[CPVarInt], nVehicles : Int) extends Constraint(cp, "Vehicle") {

  private val FAIL    = CPOutcome.Failure
  private val SUSPEND = CPOutcome.Suspend
  
  private val nSites     = pred.size
  private val nCustomers = nSites - 2*nVehicles
  
  private val Sites       = 0 until nSites
  private val Vehicles    = 0 until nVehicles 
  private val FirstDepots = nCustomers until nCustomers + nVehicles
  private val LastDepots  = nCustomers until nCustomers + 2*nVehicles
  
  private val custVehicles = Array.fill(nVehicles)(new ReversibleSetIndexedArray(cp, 0, nSites-1, true))

  override def setup(l: CPPropagStrength): CPOutcome = {
    initData()
    if (propagate() == FAIL) FAIL
    else {
      for (s <- Sites; if !vehicle(s).isBound) {
        vehicle(s).callValRemoveIdxWhenValueIsRemoved(this, s)
        vehicle(s).callValBindIdxWhenBind(this, s)
      }
      SUSPEND
    }
  }
  
  // Initializes custVehicles to reflect the array of variable vehicle
  private def initData() { 
    for (s <- Sites; v <- Vehicles; if vehicle(s) hasValue v) 
      custVehicles(v) insert s 
  }
  
  override def propagate(): CPOutcome = {
    for (i <- Sites) {
      if (checkSucc(i) == FAIL) return FAIL
      if (checkPred(i) == FAIL) return FAIL
    }
    SUSPEND
  }

  override def valRemoveIdx(cpvar: CPVarInt, i: Int, v: Int): CPOutcome = {        
    custVehicles(v).removeValue(i)   
    if (checkSucc(i) == FAIL) FAIL
    else if (checkPred(i) == FAIL) FAIL
    else SUSPEND
  }

  private def checkSucc(i: Int): CPOutcome = {  
    if (LastDepots contains i) SUSPEND
    else {
      for (j <- Sites; if succ(i) hasValue j) {
        if (noIntersection(i, j, vehicle(i).min)) {
          if (removeSucc(i, j) == FAIL) return FAIL
          if (removePred(j, i) == FAIL) return FAIL
        }
      }
      SUSPEND
    }
  }
  
  private def checkPred(i: Int): CPOutcome = {
    if (FirstDepots contains i) SUSPEND
    else {
      for (j <- Sites; if pred(i) hasValue j) {
        if (noIntersection(i, j, vehicle(i).min)) {
          if (removePred(i, j) == FAIL) return FAIL
          if (removeSucc(j, i) == FAIL) return FAIL
        }
      }
      SUSPEND
    }
  }
  
  private def noIntersection(i: Int, j: Int, v: Int): Boolean = {
    if (v > vehicle(i).max) true
    else if (!vehicle(i).hasValue(v)) noIntersection(i, j, v+1)
    else if (!custVehicles(v).hasValue(j)) noIntersection(i, j, v+1)
    else false
  }
    
  override def valBindIdx(cpvar: CPVarInt, i: Int): CPOutcome = {
    if (boundSuccVehicle(i) == FAIL) FAIL
    else if (boundPredVehicle(i) == FAIL) FAIL
    else SUSPEND
  }
  
  private def boundSuccVehicle(i: Int): CPOutcome = {
    if (LastDepots.contains(i)) SUSPEND
    else if (!succ(i).isBound) SUSPEND
    else vehicle(succ(i).value).assign(vehicle(i).value)    
  }
  
  private def boundPredVehicle(i: Int): CPOutcome = {
    if (FirstDepots.contains(i)) SUSPEND
    else if (!pred(i).isBound) SUSPEND
    else vehicle(pred(i).value).assign(vehicle(i).value)    
  }
  
  private def removePred(i: Int, j: Int): CPOutcome = succ(j).removeValue(i)
  private def removeSucc(i: Int, j: Int): CPOutcome = pred(j).removeValue(i)
}

