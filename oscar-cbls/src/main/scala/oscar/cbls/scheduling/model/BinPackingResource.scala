package oscar.cbls.scheduling.model

import oscar.cbls.binPacking.model.{Bin, BinPackingProblem, Item}
import oscar.cbls.binPacking.solver.BinPackingSolver
import oscar.cbls.constraints.core.ConstraintSystem
import oscar.cbls.constraints.lib.global.MultiKnapsack
import oscar.cbls.invariants.core.computation.{CBLSIntConst, CBLSIntVar, CBLSSetVar}
import oscar.cbls.invariants.lib.logic.TranslatedDenseCluster
import oscar.cbls.invariants.lib.minmax.ArgMaxArray
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.modeling.Algebra._
import oscar.cbls.objective.Objective

import scala.collection.SortedMap

/**
 * A bin packing resource is a resource that is held only at the first time unit of the activity using it
 * it is used for a certain amount, which is activity-dependent, and supposed constant
 * its specificity is that each time unit has a number of bins, and the activities starting at the same time unit
 * must fit into the bins of the time unit.
 *
 * We suppose that the bins cover the full history that is available in the planning.
 */
class BinPackingResource(planning:Planning, n:String, bins:Int => List[Int], MaxBPSteps:Int)
  extends  Resource(planning:Planning, n:String) {

  val maxBinsPerTimeUnit:Int = (0 to planning.maxDuration).foldLeft(0)((acc,t) => scala.math.max(acc,bins(t).length))

  case class ResourceAtTime(t:Int,
                            bins:Array[ShiftedBin],
                            var overallViolation:Objective = null,
                            var mostViolatedBins:CBLSSetVar=null){
  }

  class ShiftedItem(override val number:Int,
                    override val size:Int,
                    bin:CBLSIntVar, //this is the bin of he day
                    offset:CBLSIntVar //this is the offset, taken as the time unit where it happens
                     ) extends Item(number, size, bin){
    val shiftedBin = bin + (offset * maxBinsPerTimeUnit) //this is the absolute bin
  }

  class ShiftedBin(override val number:Int,
                   override val size:Int,
                   val shiftedNumber:Int) extends Bin(number, size)

  //for each activity using the resource, we have an item representing it (
  // this also keeps trac of the level of usage of the activity
  //As well as the bin to which the activity is set.
  var ActivitiesAndItems: SortedMap[Activity, ShiftedItem] = SortedMap.empty

  var itemCount = 0;
  private def newItemNumber():Int = {
    itemCount +=1
    itemCount -1
  }

  /**called by activities to register itself to the resource*/
  def notifyUsedBy(j: Activity, amount: Int) {
    require(!ActivitiesAndItems.isDefinedAt(j), "an activity cannot use the same BinPacking resource several times")
    ActivitiesAndItems += ((j, new ShiftedItem(
      number=newItemNumber(),
      size=amount,
      bin=CBLSIntVar(planning.model, name="bin of activity " + j),
      offset = j.earliestStartDate
    )))
  }

  val binCount:Int = planning.maxDuration * maxBinsPerTimeUnit

  private val binsAtAllTimes:Array[ResourceAtTime] = Array.tabulate(planning.maxDuration)(t => {
    val standardBinSizes = bins(t)
    val extendedBinSizes = standardBinSizes ::: List.fill(maxBinsPerTimeUnit - standardBinSizes.length)(0)
    val shift = t * maxBinsPerTimeUnit

    val binArray:Array[ShiftedBin] = Array.tabulate(maxBinsPerTimeUnit)(null)
    extendedBinSizes.zipWithIndex.map(
    {case (binSize:Int, index:Int) =>
      binArray(index) = new ShiftedBin(number = index,
                                       binSize,
                                       shiftedNumber = index + shift)})

    ResourceAtTime(t,binArray)
  })

  private val violationTracker = ArgMaxArray(binsAtAllTimes.map(_.overallViolation.objective))
  private val highestViolationPositions: CBLSSetVar = violationTracker

  val overShoot: CBLSIntVar = violationTracker.getMax
  def worseOverShootTime: Int = highestViolationPositions.value.firstKey

  /** This method is called by the framework before starting the scheduling
    * put anything that needs to be done after instantiation here
    */
  override def close(){

    //keeping track of which activity starts where through a cluster invariant
    val activityArray:Array[Activity] = ActivitiesAndItems.keys.toArray

    //setting the use, which keeps track of which activity uses the resource at any time slot
    TranslatedDenseCluster(activityArray.map(_.earliestStartDate), activityArray.map(_.ID), use)

    val binArray:Array[Bin] = Array.fill(binCount)(null)
    for(resourceAtTime <- binsAtAllTimes){
      for(bin <- resourceAtTime.bins)
        binArray(bin.number) = bin
    }

    val sc = ConstraintSystem(planning.model)

    val mkp = MultiKnapsack(
      activityArray.map((a:Activity) => ActivitiesAndItems(a).bin),
      activityArray.map((a:Activity) => CBLSIntConst(ActivitiesAndItems(a).size)),
      binArray.map(bin => bin.size).map((i:Int) => CBLSIntConst(i)))

    sc.post(mkp)

    for(resourceAtTime <- binsAtAllTimes) {
      for (bin <- resourceAtTime.bins) {
        bin.violation = mkp.violationOfBin(bin.number)
        bin.items = mkp.itemsInBin(bin.number)
      }
    }

    for(r <- binsAtAllTimes){
      r.overallViolation = Objective(Sum(r.bins.map(_.violation)))
      r.mostViolatedBins = ArgMaxArray(r.bins.map(_.violation))
    }
  }

  /** This method builds a bin packing problem regrouping the items etc.
    * of a bin packing happening at the given point in time
    */
  private def getBinPackingProblem(t:Int):BinPackingProblem = {
    val activitiesStartingAtT:Iterable[Activity] = use(t).value.map((activityID:Int) => planning.activityArray(activityID))

    BinPackingProblem(
      activitiesStartingAtT.map((a:Activity) => {val item = ActivitiesAndItems(a); (item.number,item)}).toMap,
      binsAtAllTimes(t).bins.map((b:Bin) => (b.number,b)).toMap,
      binsAtAllTimes(t).overallViolation,
      binsAtAllTimes(t).mostViolatedBins)
  }

  override def toAsciiArt(headerLength: Int): String = ""

  /** these are the activities that you can use for ejecting one of the conflicting activities */
  override def baseActivityForEjection(t: Int): Iterable[Activity] = null

  /** you need to eject one of these to solve the conflict
    * this can be null if the problem is actually solved in between, or if the problem cannot be solved */
  override def conflictingActivities(t: Int): Iterable[Activity] = {

    val binPackingProblem = getBinPackingProblem(t)
    BinPackingSolver.solveBinPacking(binPackingProblem, 4)

    //either the problem can be solved, or all activities could be moved later
    if(binPackingProblem.overallViolation.value == 0)
      return null

    use(t).value.map((activityID:Int) => planning.activityArray(activityID))
  }
}
