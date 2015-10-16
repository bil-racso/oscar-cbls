package oscar.cp.constraints

import oscar.algo.SortUtils._
import oscar.cp.core.CPOutcome._
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.{CPOutcome, CPPropagStrength, Constraint}

/**
 * Created by saschavancauwelaert on 09/10/15.
 */

//Counting-based all-different propagation, from "A Parallel, Backjumping Subgraph Isomorphism Algorithm using Supplemental Graphs" (CP2015)
class CountingBasedAllDifferent(constrainedVariables: Array[CPIntVar]) extends Constraint(constrainedVariables(0).store){

  //TODO : incremental size of bit sets depending on (min and max of all variables)

  //general variables
  private[this] val numberOfVariables = constrainedVariables.length
  private[this] val minValueOfAllDomains = constrainedVariables.map(_.min).min
  private[this] val maxValueOfAllDomains = constrainedVariables.map(_.max).max
  private[this] val maximumDomainCardinality = maxValueOfAllDomains - minValueOfAllDomains + 1
  private[this] val numberOfBuckets : Int = maximumDomainCardinality/64 + 1 //bucketIndexForValue(maximumDomainCardinality) + 1
  private[this] var currentVariableIndex : Int = -1
  private[this] var currentBucketIndex : Int = -1
  /*private[this] var numberOfBoundVariables : ReversibleInt = new ReversibleInt(constrainedVariables(0).store, 0)*/


  //set variables
  private[this] val detectedHallSetUnion : Array[Long] = Array.fill(numberOfBuckets)(0L) //H in the paper
  private[this] val currentUnionOfDomains : Array[Long] = Array.fill(numberOfBuckets)(0L) //A in the paper
  private[this] var numberOfDomainsContributingToUnion : Int = 0 //n in the paper
  private[this] var currentUnionDomainCardinality : Int = -1

  //internal domain variable (i.e. domain as bit sets)
  private[this] val bitSetDomains = Array.fill(numberOfVariables,numberOfBuckets)(0L) //domains with a bitset representation
  private[this] val bitSetDomainsBeforePropagation = Array.fill(numberOfVariables,numberOfBuckets)(0L) //domains before propagation, used to prevent removal of values already removed
  private[this] val temporalDomainValues = Array.ofDim[Int](maximumDomainCardinality) //temporal array used to fill the internal bit sets as well as to get the values to remove from the real domains
  private[this] var currentDomainElementIndex = -1
  private[this] var currentDomain : Array[Long] = null
  private[this] var isCurrentDomainEmpty : Boolean = false

  //variables for ordering
  private[this] val orderedVariablesIds : Array[Int] = Array.tabulate(numberOfVariables)(i => i)
  private[this] val domainSizes = Array.tabulate(numberOfVariables)(constrainedVariables(_).size)
  private[this] var currentOrderedVariableId : Int = -1

  final override def setup(l: CPPropagStrength): CPOutcome = {
    if (propagate() == Failure) Failure
    else {
      var i = numberOfVariables
      while (i > 0) {
        i -= 1
        constrainedVariables(i).callPropagateWhenDomainChanges(this)
      }
      Suspend
    }
  }

  final override def propagate(): CPOutcome = {

    //initialization
    numberOfDomainsContributingToUnion = 0
    clearUnionSets()

    //sort by cardinality and set bit set domains as variable domains
    currentVariableIndex = numberOfVariables
    while (currentVariableIndex > 0) {
      currentVariableIndex -= 1
      domainSizes(currentVariableIndex) = constrainedVariables(currentVariableIndex).size
      //TODO : do not clearAndFill if domain size did not change compared to last call
      currentDomain = bitSetDomains(currentVariableIndex)
      clearAndFillCurrentBitSetDomain()
    }
    mergeSort(orderedVariablesIds, domainSizes)

    //loop on variables by non-decreasing domain cardinality
    while(currentVariableIndex < numberOfVariables) {
      currentOrderedVariableId = orderedVariablesIds(currentVariableIndex)
      //TODO : maintain reversible number of bound variables and skip them as they have been propagated during last call
      currentDomain = bitSetDomains(currentOrderedVariableId)
      numberOfDomainsContributingToUnion += 1
      removeHallSetFromCurrentDomainAndUpdateDomainUnion()

      if(isCurrentDomainEmpty || currentUnionDomainCardinality < numberOfDomainsContributingToUnion)
        return CPOutcome.Failure

      if(currentUnionDomainCardinality == numberOfDomainsContributingToUnion) {
        updateHallSetAndClearDomainUnion()
        numberOfDomainsContributingToUnion = 0
      }

      currentVariableIndex += 1
    }

    updateDomainsWithBitSetDomains()

    CPOutcome.Suspend
  }

  /* Bits operations */
  @inline private def bucketIndexForValue(value : Int) = (value - minValueOfAllDomains) >>> 6
  @inline private def bitNumberInBucketForValue(value : Int) = (value - minValueOfAllDomains) & 63

  @inline private def clearAndFillCurrentBitSetDomain() : Unit = {
    currentBucketIndex = numberOfBuckets
    //clear domains
    while(currentBucketIndex > 0) {
      currentBucketIndex -= 1
      currentDomain(currentBucketIndex) = 0L
    }
    //fill domains with current values
    currentDomainElementIndex = constrainedVariables(currentVariableIndex).fillArray(temporalDomainValues)
    while(currentDomainElementIndex > 0) {
      currentDomainElementIndex -= 1
      addToBitSetDomain(temporalDomainValues(currentDomainElementIndex))
    }

    //save the domains before propagation to prevent removal of values already removed
    currentBucketIndex = numberOfBuckets
    //clear domains
    while(currentBucketIndex > 0) {
      currentBucketIndex -= 1
      bitSetDomainsBeforePropagation(currentVariableIndex)(currentBucketIndex) = currentDomain(currentBucketIndex)
    }
  }
  @inline private def clearUnionSets() : Unit = {
    currentBucketIndex = numberOfBuckets
    //clear H and A sets
    while(currentBucketIndex > 0) {
      currentBucketIndex -= 1
      detectedHallSetUnion(currentBucketIndex) = 0L
      currentUnionOfDomains(currentBucketIndex) = 0L
    }
  }
  @inline private def addToBitSetDomain(value : Int) : Unit = currentDomain(bucketIndexForValue(value)) |= (1L << bitNumberInBucketForValue(value))

  @inline private def removeHallSetFromCurrentDomainAndUpdateDomainUnion() : Unit = {
    isCurrentDomainEmpty = true
    currentUnionDomainCardinality = 0
    currentBucketIndex = numberOfBuckets
    while(currentBucketIndex > 0) {
      currentBucketIndex -= 1
      currentDomain(currentBucketIndex) &= ~detectedHallSetUnion(currentBucketIndex) //update domain
      if(currentDomain(currentBucketIndex) != 0)
        isCurrentDomainEmpty = false
      currentUnionOfDomains(currentBucketIndex) |= currentDomain(currentBucketIndex) //update domain union
      currentUnionDomainCardinality += java.lang.Long.bitCount(currentUnionOfDomains(currentBucketIndex))
    }
  }

  @inline private def updateHallSetAndClearDomainUnion() : Unit = {
    currentBucketIndex = numberOfBuckets
    while(currentBucketIndex > 0) {
      currentBucketIndex -= 1
      detectedHallSetUnion(currentBucketIndex) |= currentUnionOfDomains(currentBucketIndex) //update hall set with current domain union
      currentUnionOfDomains(currentBucketIndex) = 0L //clear domain union
    }
  }

  @inline private def updateDomainsWithBitSetDomains() : Unit = {
    currentVariableIndex = numberOfVariables
    while(currentVariableIndex > 0) {
      currentVariableIndex -= 1
      currentDomain = bitSetDomains(currentVariableIndex)

      //get the values by bucket and remove them from the real domain of the current variable
      currentBucketIndex = numberOfBuckets
      var numberOfValuesToRemove = 0
      while(currentBucketIndex > 0) {
        currentBucketIndex -= 1

        var valuesToBeRemovedBitSet = currentDomain(currentBucketIndex) ^ bitSetDomainsBeforePropagation(currentVariableIndex)(currentBucketIndex)

        //TODO : for now, linear to get the values, could be done more efficiently with a lookup table
        var bitIndex = 0
        var unaryBitSet = 0L // number of the form 0* 1 0*

        while(valuesToBeRemovedBitSet != 0) {
          unaryBitSet = 1L << bitIndex
          if((valuesToBeRemovedBitSet & unaryBitSet) != 0) {
            temporalDomainValues(numberOfValuesToRemove) = minValueOfAllDomains + 64 * currentBucketIndex + bitIndex
            numberOfValuesToRemove += 1
            valuesToBeRemovedBitSet ^= unaryBitSet
          }
          bitIndex += 1
        }
      }

      if(numberOfValuesToRemove > 0)
        constrainedVariables(currentVariableIndex).removeValues(temporalDomainValues, numberOfValuesToRemove) //do not have to check for failure as domain wipe out would have been detected before
    }
  }
}
