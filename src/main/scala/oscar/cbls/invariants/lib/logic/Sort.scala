/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/


package oscar.cbls.invariants.lib.logic
/**This package proposes a set of logic invariants, which are used to define the structure of the problem*/


import collection.immutable.{SortedMap, SortedSet}
import oscar.cbls.invariants.core.computation._

/**maintains a sorting of the ''values'' array:
 * @param ReversePerm   i < j => values(ReversePerm(i)) < values(ReversePerm(j))
 * see method GetForwardPerm() for the forward permutation: ReversePerm(ForwardPerm(i)) == i
 * */
case class Sort(var values:Array[IntVar], ReversePerm:Array[IntVar]) extends Invariant {
  val ForwardPerm:Array[IntVar]=ReversePerm.map(i => new IntVar(this.model,0,values.size,0,"ForwardPerm"))

  for (v <- values.indices) registerStaticAndDynamicDependency(values(v),v)
  finishInitialization()

  assert(values.indices == ForwardPerm.indices)
  assert(values.indices == ReversePerm.indices)
  ReversePerm.foreach(i => i.setDefiningInvariant(this))
  ForwardPerm.foreach(i => i.setDefiningInvariant(this))

  {
    //initial sort of the variables, this is in brackets to free Sorting asap
    val Sorting:Array[Int] = values.indices.toArray.sortBy(indice => values(indice).value)
    for(i <- values.indices){
      ReversePerm(i) := Sorting(i)
      ForwardPerm(Sorting(i)) := i
    }
  }

  //returns the reverse permutation of the sort.
  def GetForwardPerm():Array[IntVar]=ForwardPerm

  @inline
  override def notifyIntChanged(v:IntVar,index:Int, OldVal:Int,NewVal:Int){
    if(NewVal > OldVal ) BubbleUp(v,index) //ca a augmente
    else BubbleDown(v,index) //ca a baisse
  }

  @inline
  private def BubbleUp(v:IntVar,index:Int){
    val PositionInInitialArray:Int = index
    while(true){
      val PositionInSorting:Int = ForwardPerm(PositionInInitialArray).getValue(true)
      if(PositionInSorting == values.indices.last) return //last position
      val ValueAbove:Int=values(ReversePerm(PositionInSorting+1).getValue(true)).value
      if(ValueAbove<v.value) swap(PositionInSorting,PositionInSorting+1)
      else return
    }
  }

  @inline
  private def BubbleDown(v:IntVar,index:Int){
    val PositionInInitialArray:Int = index
    while(true){
      val PositionInSorting:Int = ForwardPerm(PositionInInitialArray).getValue(true)
      if(PositionInSorting == 0) return //first position
      val ValueBelow:Int=values(ReversePerm(PositionInSorting-1).getValue(true)).value
      if(ValueBelow>v.value) swap(PositionInSorting,PositionInSorting-1)
      else return
    }
  }

  @inline
  private def swap(Position1:Int,Position2:Int){
    val old:Int = ReversePerm(Position1).getValue(true)
    ReversePerm(Position1) := ReversePerm(Position2).getValue(true)
    ReversePerm(Position2) := old

    ForwardPerm(ReversePerm(Position1).getValue(true)) := Position1
    ForwardPerm(ReversePerm(Position2).getValue(true)) := Position2
  }

  override def checkInternals(){
    val range = values.indices
    for(i <- range){
      assert(ReversePerm(ForwardPerm(i).getValue(true)).getValue(true) == i)
    }
    for(i <- range){
      for(j <- range){
        assert(!(i < j) || (values(ReversePerm(i).getValue(true)).value <= values(ReversePerm(j).getValue(true)).value), "Sort error: " + values.toList + " ReversePerm: " + ReversePerm.toList)
      }
    }
  }
}

/**Helper object for the sort invariant, creates the necessary variables*/
object Sort{
  /**returns the ForwardPerm for a given array
   * It instantiates an array of the appropriate size and populates it with IntVar.
   */
  def MakeSort(values:Array[IntVar]):Sort = {
    val m:Model = InvariantHelper.FindModel(values)
    val ReversePerm:Array[IntVar] = values.map(v => new IntVar(m,values.indices.start,values.indices.end, 0,"reverse_perm"))
    Sort(values,ReversePerm)
  }
}
