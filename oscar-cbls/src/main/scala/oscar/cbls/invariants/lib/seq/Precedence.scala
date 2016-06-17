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

import oscar.cbls.invariants.core.algo.quick.QList
import oscar.cbls.invariants.core.algo.rb.RedBlackTreeMap
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
  private val beforesToPrecedences : Array[QList[Int]] = Array.fill(seq.maxValue + 1)(null)
  private val aftersToPrecedences : Array[QList[Int]] = Array.fill(seq.maxValue + 1)(null)
  for (precedenceID <- precedencesArray.indices) {
    val (fromValue, toValue) = precedencesArray(precedenceID)
    beforesToPrecedences(fromValue) = QList(precedenceID, beforesToPrecedences(fromValue))
    aftersToPrecedences(toValue) = QList(precedenceID, aftersToPrecedences(toValue))
  }

  //this is the exact info
  private val isPrecedenceViolated : Array[Boolean] = Array.fill(nbPecedences)(false)
  //TODO: initialize this!
  private var valueToViolatedPrecedencesOverApproximated : RedBlackTreeMap[QList[Int]] = RedBlackTreeMap.empty //this over-approximates, to speed up cleaning


  def computeAndAffectViolationsFromScratch(seq : IntSequence) {
    //this assumes that we have lots of precedence constraints, so that we can afford crawling through the sequence (instead of iterating through the precedences)
    var totalViolation = 0
    val hasValueBeenSeen = Array.fill(this.seq.maxValue + 1)(false)

    for (violatedPrecedenceList <- valueToViolatedPrecedencesOverApproximated.values) {
      for (precedenceID <- violatedPrecedenceList) {
        isPrecedenceViolated(precedenceID) = false
      }
    }
    valueToViolatedPrecedencesOverApproximated = RedBlackTreeMap.empty

    var explorerOpt = seq.explorerAtPosition(0)

    while (explorerOpt match {
      case None => false
      case Some(explorer) =>

        val value = explorer.value
        val position = explorer.position
        //we
        hasValueBeenSeen(value) = true

        var precedencesThatShouldOpenNow = beforesToPrecedences(value)
        while (precedencesThatShouldOpenNow != null) {
          val precedenceID = precedencesThatShouldOpenNow.head
          precedencesThatShouldOpenNow = precedencesThatShouldOpenNow.tail
          val shouldBeAfter = precedencesArray(precedenceID)._2
          if (hasValueBeenSeen(shouldBeAfter)) {
            //the precedence is violated!

            registerViolationIntoMaps(precedenceID)

            isPrecedenceViolated(precedenceID) = true
            totalViolation += 1
          }
        }
        explorerOpt = explorer.next
        true
    }) {}

    this := totalViolation
  }


  def registerViolationIntoMaps(precedenceID:Int) {
    //might insert duplicates!

    val (startValue,endValue) = precedencesArray(precedenceID)
    valueToViolatedPrecedencesOverApproximated.insert(startValue,
      QList(precedenceID, valueToViolatedPrecedencesOverApproximated.getOrElse(startValue, null)))

    valueToViolatedPrecedencesOverApproximated.insert(endValue,
      QList(precedenceID, valueToViolatedPrecedencesOverApproximated.getOrElse(endValue, null)))
  }



  override def notifySeqChanges(v : ChangingSeqValue, d : Int, changes : SeqUpdate) {
    if (!digestUpdates(changes, false)) {
      computeAndAffectViolationsFromScratch(changes.newValue)
    }
  }

  private def digestUpdates(changes : SeqUpdate, skipNewCheckpoints : Boolean) : Boolean = {
    changes match {
      case SeqUpdateDefineCheckpoint(prev : SeqUpdate, isActive : Boolean) =>
        digestUpdates(prev, true)
      //TODO

      case x@SeqUpdateRollBackToCheckpoint(checkpoint) =>
        digestUpdates(x.howToRollBack, true)
      //TODO

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
                isPrecedenceViolated(precedenceToCheck) = true
                this :+= 1

                registerViolationIntoMaps(precedenceToCheck)

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
                require(isPrecedenceViolated(precedenceToCheck) == false) //because was not in!
                isPrecedenceViolated(precedenceToCheck) = true
                this :+= 1
                registerViolationIntoMaps(precedenceToCheck)

              } // else precedence is not violated
          }
        }
        true

      case x@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        if (!digestUpdates(prev, skipNewCheckpoints)) return false

        valueToViolatedPrecedencesOverApproximated.get(value) match {
          case None => //no precedence to check there
          case Some(x) =>
            valueToViolatedPrecedencesOverApproximated = valueToViolatedPrecedencesOverApproximated.remove(value)
            //they are all fine now
            require(x != null)
            var precedencesInvolvingRemovedThatMustBeChecked = x
            while (x != null) {
              val precedenceToClean = precedencesInvolvingRemovedThatMustBeChecked.head
              precedencesInvolvingRemovedThatMustBeChecked = precedencesInvolvingRemovedThatMustBeChecked.tail
              if (isPrecedenceViolated(precedenceToClean)) {
                isPrecedenceViolated(precedenceToClean) = false
                this :-= 1
              }
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
            var precedencesStartedAtValue:QList[Int] = this.beforesToPrecedences(value)
            while(precedencesStartedAtValue != null){
              val precedenceToCheck = precedencesStartedAtValue.head
              precedencesStartedAtValue = precedencesStartedAtValue.tail
              val endValueOfPrecedence = precedencesArray(precedenceToCheck)._2

              if(valuesInFlip.contains(endValueOfPrecedence)){
                //the violation of this precedence is inverted

                if(isPrecedenceViolated(precedenceToCheck)){
                  //was violated, so flip to false
                  isPrecedenceViolated(precedenceToCheck) = false
                  this :-=1
                }else{
                  //was not violated, now it is violated
                  isPrecedenceViolated(precedenceToCheck) = true
                  registerViolationIntoMaps(precedenceToCheck)
                  this :+=1
                }
              }//else we do not have to check anything :-)
            }
          }
          true
        } else {
          //move with flip

          //TODO: also get the positions straight!
          val valuesInMovedSegment : SortedSet[Int] = prev.newValue.valuesBetweenPositions(fromIncluded, toIncluded)
          for (value <- valuesInMovedSegment) {

            //violations involved in value
            var precedencesStartedAtValue : QList[Int] = this.beforesToPrecedences(value)
            while (precedencesStartedAtValue != null) {
              val precedenceToCheck = precedencesStartedAtValue.head
              precedencesStartedAtValue = precedencesStartedAtValue.tail
              val endValueOfPrecedence = precedencesArray(precedenceToCheck)._2

              if (valuesInMovedSegment.contains(endValueOfPrecedence)) {
                //the violation of this precedence is inverted (only do this for one half of the precedence)

                if (flip) {
                  if (isPrecedenceViolated(precedenceToCheck)) {
                    //was violated, so flip to false
                    isPrecedenceViolated(precedenceToCheck) = false
                    this :-= 1
                  } else {
                    //was not violated, now it is violated
                    isPrecedenceViolated(precedenceToCheck) = true
                    registerViolationIntoMaps(precedenceToCheck)
                    this :+= 1
                  }
                }
              } else {
                //the other one is not in the flipped move, so we have to check the impact of the move on this precedence

                changes.newValue.positionOfAnyOccurrence(endValueOfPrecedence) match {
                  case None => //other not present in the sequence, nothing to do
                  case Some(newPositionOfEndValueOfPrecedence) =>
                    val newPositionOfStartValueOfPrecedence = changes.newValue.positionOfAnyOccurrence(value).head

                    if (newPositionOfStartValueOfPrecedence < newPositionOfEndValueOfPrecedence) {
                      //no violation after move
                      if (isPrecedenceViolated(precedenceToCheck)) {
                        //was violated, so flip to false
                        isPrecedenceViolated(precedenceToCheck) = false
                        this :-= 1
                      } //was not violated, nothing to do

                    } else {
                      //violation after move
                      if (!isPrecedenceViolated(precedenceToCheck)) {
                        //was not violated, now it is violated
                        isPrecedenceViolated(precedenceToCheck) = true
                        registerViolationIntoMaps(precedenceToCheck)
                        this :+= 1
                      }
                    }
                }
              }
            }

            var precedencesEndedAtValue = this.aftersToPrecedences(value)
            while (precedencesEndedAtValue != null) {
              val precedenceToCheck = precedencesEndedAtValue.head
              precedencesEndedAtValue = precedencesEndedAtValue.tail
              val startValueOfPrecedence = precedencesArray(precedenceToCheck)._1

              if (!valuesInMovedSegment.contains(startValueOfPrecedence)) {

                //the other one is not in the flipped move, so we have to check the impact of the move on this precedence

                changes.newValue.positionOfAnyOccurrence(startValueOfPrecedence) match {
                  case None => //other not present in the sequence, nothing to do
                  case Some(newPositionOfStartValueOfPrecedence) =>
                    val newPositionOfEndValueOfPrecedence = changes.newValue.positionOfAnyOccurrence(value).head

                    if (newPositionOfStartValueOfPrecedence < newPositionOfEndValueOfPrecedence) {
                      //no violation after move
                      if (isPrecedenceViolated(precedenceToCheck)) {
                        //was violated, so flip to false
                        isPrecedenceViolated(precedenceToCheck) = false
                        this :-= 1
                      } //was not violated, nothing to do

                    } else {
                      //violation after move
                      if (!isPrecedenceViolated(precedenceToCheck)) {
                        //was not violated, now it is violated
                        isPrecedenceViolated(precedenceToCheck) = true
                        registerViolationIntoMaps(precedenceToCheck)
                        this :+= 1
                      }
                    }
                }
              }
            }
          }
        }
        true


      case SeqUpdateLastNotified(value : IntSequence) =>
        true //we are starting from the previous value

      case SeqUpdateSet(value : IntSequence) =>
        false //impossible to go incremental
    }
  }
}