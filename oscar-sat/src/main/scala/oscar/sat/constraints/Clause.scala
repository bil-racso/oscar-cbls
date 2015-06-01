package oscar.sat.constraints

import oscar.sat.core.CDCLStore
import scala.collection.mutable.ArrayBuffer
import oscar.sat.core.True
import oscar.sat.core.False
import oscar.algo.array.ArrayStackInt


object Clause {
  
  def apply(literals: Array[Int], store: CDCLStore): Clause = {
    ???
  }
  
  def learnt(literals: Array[Int], store: CDCLStore): Clause = {
    ???
  }
  
}

abstract class Clause extends Constraint {
  
}
