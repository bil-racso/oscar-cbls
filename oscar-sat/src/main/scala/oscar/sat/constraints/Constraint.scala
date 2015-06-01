package oscar.sat.constraints

import oscar.algo.array.ArrayStackInt
import oscar.sat.core.CDCLStore

abstract class Constraint {

  def deleted: Boolean
  
  def delete(): Unit
  
  def explainAssign(reason: ArrayStackInt): Unit
  
  def explainFail(reason: ArrayStackInt): Unit

  def propagate(litId: Int): Boolean
}