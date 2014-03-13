package oscar.cbls.search

import oscar.cbls.invariants.core.computation.{CBLSIntVar, CBLSSetVar}

/**
 * Created by rdl on 13/03/14.
 */
object BinPackingSolver {
  def solveBinPacking(items:List[Item], bins: List[Bin], overallViolation:CBLSIntVar):Boolean = {


  }
}

case class Item(number:Int,
                 size:Int,
                 bin: CBLSIntVar)

case class Bin(number:Int,
               size:Int,
               var items:CBLSSetVar = null,
               var violation:CBLSIntVar = null)
