/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/
package oscar.cbls.search.binPacking

import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.computation.CBLSIntConst
import oscar.cbls.objective.Objective
import oscar.cbls.constraints.lib.global.MultiKnapsack
import oscar.cbls.invariants.core.computation.Store
import oscar.cbls.constraints.core.ConstraintSystem
import oscar.cbls.invariants.lib.minmax.ArgMaxArray
import scala.collection.immutable.SortedMap

case class Item(number:Int,
                size:Int,
                var bin: CBLSIntVar = null){
  override def toString: String = "Item(nr:" + number + " size:" + size + " bin:" + bin.value + ")"
}

case class Bin(number:Int,
               size:Int,
               var items:CBLSSetVar = null,
               var violation:CBLSIntVar = null){
  override def toString: String = "Bin(nr:" + number + " size:" + size + " items:" + items.valueString + " viol:" + violation.value +")"
}

case class BinPackingProblem(items:Map[Int,Item],
                             bins: Map[Int,Bin],
                             overallViolation:Objective,
                             mostViolatedBins:CBLSSetVar){
  override def toString: String =
    "BinPackingProblem(\n\titems:{" + items.values.mkString(",") +"}\n" +
      "\tbins:{" +bins.values.mkString(",") + "}\n" +
      "\toverallViolation:" + overallViolation.Objective.value + "\n" +
      "\tmostViolatedBins:" + mostViolatedBins.valueString+")"

  def itemCount = items.size
  def binCount = bins.size
}

object BinPackingProblem{

  private def arrayToIndexElementList[T](a:Array[T]):Map[Int,T]={
    var toReturn:SortedMap[Int,T] = SortedMap.empty
    for(i <- a.indices){
      toReturn += ((i,a(i)))
    }
    toReturn
  }

  private def listToIndexElementList[T](l:List[T]):Map[Int,T]={
    var toReturn:SortedMap[Int,T] = SortedMap.empty
    var i = 0
    for(e <- l){
      toReturn += ((i,e))
      i += 1
    }
    toReturn
  }

  def apply(items:Array[Item],
            bins:Array[Bin],
            overallViolation:Objective,
            mostViolatedBins:CBLSSetVar):BinPackingProblem = {


    new BinPackingProblem(arrayToIndexElementList(items),
      arrayToIndexElementList(bins),
      overallViolation,
      mostViolatedBins)

  }


  def apply(itemSize:Iterable[Int], binSizes:Iterable[Int], s:Store, c:ConstraintSystem, initialBin:Int):BinPackingProblem = {
    apply(itemSize.toArray, binSizes.toArray, s, c, initialBin)
  }

  /** this method also posts the constraints and invariants involved in the BinPackingProblem
    *
    * @param binSizesArray
    * @param itemSizeArray
    * @param s
    * @param c
    * @return
    */
  def apply(itemSizeArray:Array[Int], binSizesArray:Array[Int], s:Store, c:ConstraintSystem, initialBin:Int):BinPackingProblem = {

    val binArray: Array[Bin] = Array.tabulate(binSizesArray.size)(
      binNumber => Bin(binNumber,
        binSizesArray(binNumber)))

    val itemArray = Array.tabulate(itemSizeArray.size)(
      itemNumber => Item(itemNumber,
        itemSizeArray(itemNumber),
        CBLSIntVar(s, 0 to (binArray.size-1), initialBin, "bin of item " + itemNumber)))

    val mkp = MultiKnapsack(itemArray.map(_.bin),
      itemSizeArray.map(itemSize => CBLSIntConst(itemSize)),
      binSizesArray.map(binSize => CBLSIntConst(binSize)))

    c.post(mkp)

    for (bin <- binArray) {
      bin.violation = mkp.violationOfBin(bin.number)
      bin.items = mkp.itemsInBin(bin.number)
    }

    BinPackingProblem(itemArray,
      binArray,
      Objective(mkp.violation),
      ArgMaxArray(binArray.map(_.violation)))
  }
}
