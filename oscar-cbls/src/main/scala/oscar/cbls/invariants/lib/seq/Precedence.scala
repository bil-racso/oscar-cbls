package oscar.cbls.invariants.lib.seq

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
/*

import oscar.cbls.invariants.core.algo.quick.QList
import oscar.cbls.invariants.core.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation._

import scala.collection.immutable.{SortedMap, SortedSet}

//CBLSSetVar(model,name="values involved in violation of precedence")

/**
 * precedence assumes that number can occur only once in the sequence
 * so that the constraint is to be enforced from any occurrence to any occurrence,
 * "any" being chosen arbitrarily by tne invariant, and the choice an of course change at any time.
 * also if one of the two value of a precedence is not present in the sequence,
 * it is considered that the precedence is enforced.
 * @param seq
 * @param beforeAfter
 */
class Precedence(seq:ChangingSeqValue,
                 beforeAfter:List[(Int,Int)],
                 violation:CBLSIntVar)
  extends Invariant()
  with SeqNotificationTarget{

  registerStaticAndDynamicDependency(seq)
  violation.setDefiningInvariant(this)
  finishInitialization()

  //saving precedences into arrays
  private val precedencesArray:Array[(Int,Int)] = beforeAfter.toArray
  private val beforesToPrecedences:Array[QList[Int]] = Array.fill(seq.maxValue+1)(null)
  private val aftersToPrecedences:Array[QList[Int]]  = Array.fill(seq.maxValue+1)(null)
  for(precedenceID <- precedencesArray.indices){
    val (fromValue,toValue) = precedencesArray(precedenceID)
    beforesToPrecedences(fromValue) = QList(precedenceID,beforesToPrecedences(fromValue))
    aftersToPrecedences(toValue) = QList(precedenceID,aftersToPrecedences(toValue))
  }

  private var violatedPrecedencesToPositions=SortedMap.empty[Int,(Int,Int)]
  {
    val (viol,violated) = computeViolatedFromScratch(seq.value)
    violatedPrecedencesToPositions = violated
    violation := viol
  }

  var tmpArrayOfValueToIntM1WhenNotInUse = Array.fill[Int](seq.maxValue+1)(-1)
  /**
   * @param seq
   * @return the degree of violation, and the set of violated Precedences ID to the position of the before and after values
   */
  def computeViolatedFromScratch(seq:IntSequence):(Int,SortedMap[Int,(Int,Int)]) = {
    var explorer = seq.explorerAtPosition(0)
    //where the precedences have been closed

    val valueToPositionM1IfNotSeen = tmpArrayOfValueToIntM1WhenNotInUse
    var toClean:QList[Int] = null

    var violationDegree:Int = 0
    var violatedPrecedences=SortedMap.empty[Int,(Int,Int)]

    while(explorer match{
      case None => false
      case Some(seqPosition) =>
        val value = seqPosition.value
        val position = seqPosition.position
        explorer = seqPosition.next
        //check that all values are in open, and close them all.
        //for values that are not in open,

        //does it open some already closed precedences?
        for(precedenceIDOpenedByValue <- beforesToPrecedences(value)){
          val valueShouldComeAfter = precedencesArray(precedenceIDOpenedByValue)._2
          val positionOfValueShouldComeAfter = valueToPositionM1IfNotSeen(valueShouldComeAfter)
          if(positionOfValueShouldComeAfter != -1){
            //value has already been encountered, so it is a bug!
            val violationOfPrecedence = (position - positionOfValueShouldComeAfter)
            violationDegree += violationOfPrecedence
            violatedPrecedences = violatedPrecedences + ((precedenceIDOpenedByValue,(position,positionOfValueShouldComeAfter)))
          }
        }
        require(valueToPositionM1IfNotSeen(value) == -1)
        valueToPositionM1IfNotSeen(value) = position
        toClean = QList(value,toClean)
        true
    }){}

    //now, we clean the array
    for(value <- toClean) valueToPositionM1IfNotSeen(value) = -1

    (violationDegree,violatedPrecedences)
  }

  /**
   * @param violatedPrecedences
   * @return value -> precedences where it appears
   */
  def violatedPrecedencesToViolatedValues(violatedPrecedences:SortedMap[Int,(Int,Int)]):SortedMap[Int,Int] = {
    var toReturn = SortedMap.empty[Int,Int]
    def addToViolatedValueToToReturn(value:Int){
      toReturn = toReturn + ((value,toReturn.getOrElse(value,0)))
    }
    for(precedenceID <- violatedPrecedences.keys){
      val prec = this.precedencesArray(precedenceID)
      addToViolatedValueToToReturn(prec._1)
      addToViolatedValueToToReturn(prec._2)
    }
    toReturn
  }

  override def notifySeqChanges(v : ChangingSeqValue, d : Int, changes : SeqUpdate){


  }


  private def movedValues(changes:SeqUpdate):Option[SortedSet]= {
    val newValue = changes.newValue

    changes match {
      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        if(!digestUpdates(prev)) return false

        true
      case x@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        if(!digestUpdates(prev)) false
        else if(x.isNop) true
        else if(x.isSimpleFlip){

          true
        }else {

          true
        }

      case x@SeqUpdateRemove(position: Int, prev : SeqUpdate) =>
        //on which vehicle did we remove?
        //on which vehicle did we insert?
        if(!digestUpdates(prev)) return false

        true
      case SeqUpdateSet(value : IntSequence) =>


        false //impossible to go incremental
      case SeqUpdateLastNotified(value:IntSequence) =>
        true //we are starting from the previous value
      case SeqUpdateRollBackToCheckpoint(checkpoint) =>
        if(checkpoint == null) false //it has been dropped following a Set
        else {

          true
        }
    }
  }
}
*/