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

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker

import scala.collection.immutable.SortedSet

//CBLSSetVar(model,name="values involved in violation of precedence")

/**
 * precedence assumes that number can occur only once in the sequence
 * so that the constraint is to be enforced from any occurrence to any occurrence,
 * "any" being chosen arbitrarily by tne invariant, and the choice an of course change at any time.
 * also if one of the two value of a precedence is not present in the sequence,
 * it is considered that the precedence is enforced.
 *
 * maintains the number of violated precedences.
 *
 * @param seq
 * @param beforeAfter
 */
class Precedence(seq:ChangingSeqValue,
                 beforeAfter:List[(Int,Int)]) //supposed to have zero redundancies.
  extends IntInvariant()
  with SeqNotificationTarget {

  registerStaticAndDynamicDependency(seq)
  finishInitialization()

  //saving precedences into arrays
  private val precedencesArray : Array[(Int, Int)] = beforeAfter.toArray
  val nbPecedences = precedencesArray.length
  val precedences = 0 until nbPecedences

  private val beforesToPrecedences : Array[QList[Int]] = Array.fill(seq.maxValue + 1)(null)
  private val aftersToPrecedences : Array[QList[Int]] = Array.fill(seq.maxValue + 1)(null)
  for (precedenceID <- precedencesArray.indices) {
    val (fromValue, toValue) = precedencesArray(precedenceID)
    require(fromValue != toValue, "there should be no self precedence!")
    beforesToPrecedences(fromValue) = QList(precedenceID, beforesToPrecedences(fromValue))
    aftersToPrecedences(toValue) = QList(precedenceID, aftersToPrecedences(toValue))
  }



  private val isPrecedenceViolated : Array[Boolean] = Array.fill(nbPecedences)(false)

  private val isViolationChangedSinceCheckpoint:Array[Boolean] = Array.fill(nbPecedences)(false)
  private var changedPrecedenceViolationsSinceCheckpoint:QList[Int] = null
  private val savedViolationAtCheckpoint:Array[Boolean] = Array.fill(nbPecedences)(false)
  private var checkpoint:IntSequence = null
  var violationAtCheckpoint:Int = -1

  def saveViolationForCheckpoint(precedence:Int){
    if(!isViolationChangedSinceCheckpoint(precedence)){
      isViolationChangedSinceCheckpoint(precedence) = true
      changedPrecedenceViolationsSinceCheckpoint = QList(precedence,changedPrecedenceViolationsSinceCheckpoint)
      savedViolationAtCheckpoint(precedence) = isPrecedenceViolated(precedence)
    }
  }

  def reloadViolationsAtCheckpoint(){
    for(precedence <- changedPrecedenceViolationsSinceCheckpoint){
      isViolationChangedSinceCheckpoint(precedence) = false
      isPrecedenceViolated(precedence) = savedViolationAtCheckpoint(precedence)
    }
    changedPrecedenceViolationsSinceCheckpoint = null
    this := violationAtCheckpoint
  }

  def defineCheckpoint(i:IntSequence){
    for(precedence <- changedPrecedenceViolationsSinceCheckpoint) {
      isViolationChangedSinceCheckpoint(precedence) = false
    }
    changedPrecedenceViolationsSinceCheckpoint = null
    checkpoint = i
    violationAtCheckpoint = this.newValue
  }

  def clearAllViolatedPrecedences(){
    for (precedence <- precedences) {
      isPrecedenceViolated(precedence) = false
    }
  }

  def computeAndAffectViolationsFromScratch(seq : IntSequence) {
    //this assumes that we have lots of precedence constraints, so that we can afford crawling through the sequence (instead of iterating through the precedences)
    var totalViolation = 0
    val hasValueBeenSeen = Array.fill(this.seq.maxValue + 1)(false)

    clearAllViolatedPrecedences()

    var explorerOpt = seq.explorerAtPosition(0)

    while (explorerOpt match {
      case None => false
      case Some(explorer) =>

        val value = explorer.value

        hasValueBeenSeen(value) = true

        var precedencesThatShouldOpenNow = beforesToPrecedences(value)
        while (precedencesThatShouldOpenNow != null) {
          val precedenceID = precedencesThatShouldOpenNow.head
          precedencesThatShouldOpenNow = precedencesThatShouldOpenNow.tail
          val shouldBeAfter = precedencesArray(precedenceID)._2
          if (hasValueBeenSeen(shouldBeAfter)) {
            //the precedence is violated!
            isPrecedenceViolated(precedenceID) = true
            totalViolation += 1
          }
          isViolationChangedSinceCheckpoint(precedenceID) = true
        }
        explorerOpt = explorer.next
        true
    }) {}

    this := totalViolation
  }

  override def notifySeqChanges(v : ChangingSeqValue, d : Int, changes : SeqUpdate) {
    if (!digestUpdates(changes, false)) {
      computeAndAffectViolationsFromScratch(changes.newValue)
      //this also updates checkpoint links
    }
  }

  private def digestUpdates(changes : SeqUpdate, skipNewCheckpoints : Boolean) : Boolean = {
    changes match {
      case SeqUpdateDefineCheckpoint(prev : SeqUpdate, isActive : Boolean) =>
        if(!skipNewCheckpoints) {
          if(!digestUpdates(prev, true)){
            computeAndAffectViolationsFromScratch(changes.newValue)
          }
          defineCheckpoint(changes.newValue)
          true
        }else{
          digestUpdates(prev, true)
        }
      case x@SeqUpdateRollBackToCheckpoint(rollBackCheckpoint) =>
        require(checkpoint quickEquals rollBackCheckpoint)
        reloadViolationsAtCheckpoint()
        true

      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        if (!digestUpdates(prev, skipNewCheckpoints)) return false

        var precedencesStartingAtInsertedValue = beforesToPrecedences(value)
        while (precedencesStartingAtInsertedValue != null) {
          val precedenceToCheck = precedencesStartingAtInsertedValue.head
          precedencesStartingAtInsertedValue = precedencesStartingAtInsertedValue.tail

          //searching for end of precedence that is already in
          val endValueOfPrecedence = this.precedencesArray(precedenceToCheck)._2
          prev.newValue.positionOfAnyOccurrence(endValueOfPrecedence) match {
            case None => ; //the precedence is fine
            case Some(positionOfEndValueOfPrecedence) =>
              if (positionOfEndValueOfPrecedence < pos) {
                //precedence is violated!
                saveViolationForCheckpoint(precedenceToCheck)
                this :+= 1
                isPrecedenceViolated(precedenceToCheck) = true
              } // else precedence is not violated
          }
        }

        var precedencesEndingAtInsertedValue = aftersToPrecedences(value)
        while (precedencesEndingAtInsertedValue != null) {
          val precedenceToCheck = precedencesEndingAtInsertedValue.head
          precedencesEndingAtInsertedValue = precedencesEndingAtInsertedValue.tail

          //searching for end of precedence that is already in
          val startValueOfPrecedence = this.precedencesArray(precedenceToCheck)._1
          prev.newValue.positionOfAnyOccurrence(startValueOfPrecedence) match {
            case None => ; //the precedence is fine
            case Some(positionOfStartValueOfPrecedence) =>
              if (pos < positionOfStartValueOfPrecedence) {
                //precedence is violated!
                saveViolationForCheckpoint(precedenceToCheck)
                this :+= 1
                isPrecedenceViolated(precedenceToCheck) = true
              } // else precedence is not violated
          }
        }
        true

      case x@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        if (!digestUpdates(prev, skipNewCheckpoints)) return false

        for(precedencesThatShouldOpenAtValue <- beforesToPrecedences(value)){
          if (isPrecedenceViolated(precedencesThatShouldOpenAtValue)) {
            saveViolationForCheckpoint(precedencesThatShouldOpenAtValue)
            isPrecedenceViolated(precedencesThatShouldOpenAtValue) = false
            this :-= 1
          }
        }

        for(precedenceThatShouldCloseAtValue <- aftersToPrecedences(value)){
          if (isPrecedenceViolated(precedenceThatShouldCloseAtValue)) {
            saveViolationForCheckpoint(precedenceThatShouldCloseAtValue)
            isPrecedenceViolated(precedenceThatShouldCloseAtValue) = false
            this :-= 1
          }
        }

        true

      case x@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        //on which vehicle did we move?
        //also from --> to cannot include a vehicle start.
        if (!digestUpdates(prev, skipNewCheckpoints)) false
        else if (x.isNop) true
        else if (x.isSimpleFlip) {
          //this is a simple flip

          //violation is only impacted if the other value of the precedence is also in the flip.
          //in this case, it inverts the truth value of the violation.
          //two inversions will happen,
          //that'w why there is only the loop on precedences considering the precedences started at the flipped values,
          // not the ones ended at the flipped values

          val valuesInFlip:SortedSet[Int] = prev.newValue.valuesBetweenPositions(fromIncluded,toIncluded)

          for(value <- valuesInFlip){
            //violations involved in value
            for(precedenceThatShouldOpenAtValue <- this.beforesToPrecedences(value)){
              val endValueOfPrecedence = precedencesArray(precedenceThatShouldOpenAtValue)._2
              if(valuesInFlip.contains(endValueOfPrecedence)){
                //the violation of this precedence is inverted
                if(isPrecedenceViolated(precedenceThatShouldOpenAtValue)){
                  //was violated, so flip to false
                  isPrecedenceViolated(precedenceThatShouldOpenAtValue) = false
                  this :-=1
                }else{
                  //was not violated, now it is violated
                  isPrecedenceViolated(precedenceThatShouldOpenAtValue) = true
                  this :+=1
                }
              }//else we do not have to check anything :-)
            }
          }
          true

        }else{ //move with/without flip

          val moveDownwards = x.moveDownwards
          val moveUpwards = x.moveUpwards

          val valuesInMovedSegment : SortedSet[Int] = prev.newValue.valuesBetweenPositions(fromIncluded, toIncluded)
          for (value <- valuesInMovedSegment) {

            //violations involved in value
            for (precedenceStartedAtValue <- this.beforesToPrecedences(value)) {
              if (isPrecedenceViolated(precedenceStartedAtValue)) {
                //it was violated.
                //it will become non-violated if moved downwards and the other value passes up
                //or if there is a flip and the other value is in the flip

                if (moveDownwards) {
                  val endValueOfPrecedence = precedencesArray(precedenceStartedAtValue)._2
                  if (prev.newValue.positionOfAnyOccurrence(endValueOfPrecedence).head < fromIncluded) {
                    saveViolationForCheckpoint(precedenceStartedAtValue)
                    isPrecedenceViolated(precedenceStartedAtValue) = false
                    this :-= 1
                  }
                } else if (flip) {
                  val endValueOfPrecedence = precedencesArray(precedenceStartedAtValue)._2
                  if (valuesInMovedSegment.contains(endValueOfPrecedence)) {
                    //the violation of this precedence is inverted (only do this for one half of the precedence)
                    if (flip) {
                      //was violated, so flip to false
                      saveViolationForCheckpoint(precedenceStartedAtValue)
                      isPrecedenceViolated(precedenceStartedAtValue) = false
                      this :-= 1
                    }
                  }
                }

              } else {
                //it was not violated, thus is can only get violated is the segment moved upwards
                // of if the other value is also in the segment and there is a flip

                if (moveUpwards) {
                  val endValueOfPrecedence = precedencesArray(precedenceStartedAtValue)._2
                  if (prev.newValue.positionOfAnyOccurrence(endValueOfPrecedence).head <= after) {
                    saveViolationForCheckpoint(precedenceStartedAtValue)
                    isPrecedenceViolated(precedenceStartedAtValue) = true
                    this :+= 1
                  }
                } else if (flip) {
                  val endValueOfPrecedence = precedencesArray(precedenceStartedAtValue)._2
                  if (valuesInMovedSegment.contains(endValueOfPrecedence)) {
                    //the violation of this precedence is inverted (only do this for one half of the precedence)
                    if (flip) {
                      //was not violated, now it is violated
                      saveViolationForCheckpoint(precedenceStartedAtValue)
                      isPrecedenceViolated(precedenceStartedAtValue) = true
                      this :+= 1
                    }
                  }
                }
              }
            }

            for (precedenceEndingAtValue <- this.aftersToPrecedences(value)) {
              if (isPrecedenceViolated(precedenceEndingAtValue)) {
                //it was violated.
                //it will become non-violated if moved upwards and the other value passes down
                //or if there is a flip and the other value is in the flip, but the flip case is handled in the precedenceStarted at value

                if (moveUpwards) {
                  val startValueOfPrecedence = precedencesArray(precedenceEndingAtValue)._1
                  if (prev.newValue.positionOfAnyOccurrence(startValueOfPrecedence).head < after) {
                    saveViolationForCheckpoint(precedenceEndingAtValue)
                    isPrecedenceViolated(precedenceEndingAtValue) = false
                    this :-= 1
                  }
                }

              } else {
                //it was not violated, thus is can only get violated is the segment moves downwards and the other value passes up
                // of if the other value is also in the segment and there is a flip, but the flip case is handled in the precedenceStarted at value

                if (moveDownwards) {
                  val startValueOfPrecedence = precedencesArray(precedenceEndingAtValue)._1
                  if (prev.newValue.positionOfAnyOccurrence(startValueOfPrecedence).head > after) {
                    saveViolationForCheckpoint(precedenceEndingAtValue)
                    isPrecedenceViolated(precedenceEndingAtValue) = true
                    this :+= 1
                  }
                }
              }
            }
          }
          true
        }

      case SeqUpdateLastNotified(value : IntSequence) =>
        true //we are starting from the previous value

      case SeqUpdateSet(value : IntSequence) =>
        false //impossible to go incremental
    }
  }

  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c : Checker) : Unit = {
    for(precedenceID <- precedences){
      val (startValue,endValue) = precedencesArray(precedenceID)

      val s = seq.value

      var nbViol = 0
      s.positionOfAnyOccurrence(startValue) match {
        case None => ;
          c.check(!isPrecedenceViolated(precedenceID))
        case Some(positionOfStartValue) =>
          s.positionOfAnyOccurrence(endValue) match {
            case None => ;
              c.check(!isPrecedenceViolated(precedenceID))
            case Some(positionOfEndValue) =>
              c.check(isPrecedenceViolated(precedenceID) == (positionOfStartValue < positionOfEndValue))
              nbViol += 1
          }
      }

      c.check(this.value == nbViol)
    }
  }
}

