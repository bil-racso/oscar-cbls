package oscar.cbls.scheduling.model

import oscar.cbls.invariants.core.computation.{CBLSSetVar, CBLSIntConst, CBLSIntVar}
import oscar.cbls.constraints.core.ConstraintSystem
import oscar.cbls.constraints.lib.global.MultiKnapsack
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.modeling.Algebra._
import oscar.cbls.search.binPacking.{Item, BinPackingSolver, BinPackingProblem, Bin}
import oscar.cbls.invariants.lib.logic.TranslatedDenseCluster
import oscar.cbls.objective.Objective
import scala.collection.SortedMap
import oscar.cbls.invariants.lib.minmax.ArgMaxArray
import scala.collection.immutable.SortedSet
import oscar.cbls.invariants.lib.set.SetMap

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

  //for each activity using the resource, we have an item representing it (
  // this also keeps trac of the level of usage of the activity
  //As well as the bin to which the activity is set.
  var ActivitiesAndItems: SortedMap[Activity, Item] = SortedMap.empty

  var itemCount = 0;
  private def newItemNumber():Int = {
    itemCount +=1
    itemCount -1
  }

  /**called by activities to register itself to the resource*/
  def notifyUsedBy(j: Activity, amount: Int) {
    require(!ActivitiesAndItems.isDefinedAt(j), "an activity cannot use the same resource several times")
    ActivitiesAndItems += ((j, Item(
      number=newItemNumber(),
      size=amount,
      bin=CBLSIntVar(planning.model, name="overall_violation"))
      ))
  }

  private var binCount = 0;
  private def newBinNumber():Int = {
    val toReturn = binCount
    binCount += 1
    toReturn
  }

  case class ResourceAtTime(t:Int,
                            zeroBin:Bin,
                            bins:List[Bin],
                            var overallViolation:Objective = null,
                            var violationNotZero:Objective = null,
                             var mostViolatedBins:CBLSSetVar=null){
    def allBins:List[Bin] = zeroBin :: bins
  }

  var resourcesAtAllTimes:Array[ResourceAtTime] = Array.tabulate(planning.maxDuration)(t => {
    ResourceAtTime(t=t,
      zeroBin=Bin(newBinNumber(),0 /*maxSize is zero for entry bin, representing the additional phantom bin*/),
      bins=bins(t).map((binSize:Int) => Bin(newBinNumber(), binSize))
    )
  })

  /** This method is called by the framework before starting the scheduling
    * put anything that needs to be done after instantiation here
    */
  override def close(){

    //keeping track of which activity starts where through a cluster invariant
    val activityArray:Array[Activity] = ActivitiesAndItems.keys.toArray

    //setting the use, which keeps track of which activity uses the resoruce at any time slot
    TranslatedDenseCluster(activityArray.map(_.earliestStartDate), activityArray.map(_.ID), use)

    val allBins:List[Bin] = resourcesAtAllTimes.map(_.allBins).flatten.toList
    val binArray:Array[Bin] = Array.fill(binCount)(null)
    for(bin <- allBins){
      binArray(bin.number) = bin
    }

    val sc = ConstraintSystem(planning.model)

    val mkp = MultiKnapsack(activityArray.map((a:Activity) => ActivitiesAndItems(a).bin),
      activityArray.map((a:Activity) => CBLSIntConst(ActivitiesAndItems(a).size)),
      binArray.map(bin => bin.size).map((i:Int) => CBLSIntConst(i)))

    sc.post(mkp)

    for(bin <- allBins){
      bin.violation = mkp.violationOfBin(bin.number)
      bin.items = mkp.itemsInBin(bin.number)
    }

    for(r <- resourcesAtAllTimes){
      r.violationNotZero = Objective(Sum(r.bins.map(bin => bin.violation)))
      r.overallViolation = Objective(r.violationNotZero.Objective + r.zeroBin.violation)

      val binArrayForTime:Array[Bin] = r.allBins.toArray
      val binArrayToBinID = binArrayForTime.map(_.number)

      r.mostViolatedBins = ArgMaxArray(binArrayForTime.map(_.violation)).map(binArrayToBinID(_))
    }
  }

  /** This method builds a bin packing problem regrouping the items etc.
    * of a bin packing happening at the given point in time
    */
  def getBinPackingProblem(t:Int, withBinZero:Boolean):BinPackingProblem = {
    val activitiesStartingAtT:Iterable[Activity] = use(t).value.map((activityID:Int) => planning.activityArray(activityID))
    val itemsAtTimeT:Iterable[Item] = activitiesStartingAtT.map((a:Activity) => ActivitiesAndItems(a))

    BinPackingProblem(activitiesStartingAtT.map((a:Activity) => {val item = ActivitiesAndItems(a); (item.number,item)}).toMap,
      (if(withBinZero) resourcesAtAllTimes(t).allBins else resourcesAtAllTimes(t).bins).map((b:Bin) => (b.number,b)).toMap,
      if(withBinZero) resourcesAtAllTimes(t).overallViolation else resourcesAtAllTimes(t).violationNotZero,
      resourcesAtAllTimes(t)mostViolatedBins)
  }

  override def toAsciiArt(headerLength: Int): String = ""

  /** these are the activities that you can use for ejecting one of the conflicting activities */
  override def baseActivityForEjection(t: Int): Iterable[Activity] = null

  /** you need to eject one of these to solve the conflict
    * this can be null if the problem is actually solved in between, or if the problem cannot be solved */
  override def conflictingActivities(t: Int): Iterable[Activity] = null

  /** The first violation of the resource in time
    * @return
    */
  override def worseOverShootTime: Int = 0

  override val overShoot: CBLSIntVar = null
}

/*
il faut aussi savoir dans quel ordre on va faire le flatten.
Je pense qu'il vaut mieux faire les cumulative en premier, puis les binPacking.
bon, au final on va quand-même les intertwiner alors je sais aps si ça va changer grand chose.
par contre, le pense qu'il vaut mieux faire du début à la fin pour les binPAcking parce-que le déplacement va induire plus de conflits que dans le cas de cumulatives.
 */

