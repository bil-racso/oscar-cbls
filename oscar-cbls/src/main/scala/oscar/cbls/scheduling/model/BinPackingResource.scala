package oscar.cbls.scheduling.model

import oscar.cbls.invariants.core.computation.{CBLSIntConst, CBLSIntVar}
import oscar.cbls.constraints.core.ConstraintSystem
import oscar.cbls.constraints.lib.global.MultiKnapsack
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.modeling.Algebra._
import oscar.cbls.search.binPacking.{BinPackingSolver, BinPackingProblem, Bin}

/**
 * A bin packing resource is a resource that is held only at the first time unit of the activity using it
 * it is used for a certain amount, which is activity-dependent, and supposed constant
 * its specificity is that each time unit has a number of bins, and the activities starting at the same time unit
 * must fit into the bins of the time unit.
 *
 * We suppose that the bins cover the full history that is available in the planning.
 *
 */
class BinPackingResource(planning:Planning, n:String, bins:Int => List[Int], MaxBPSteps:Int) extends  Resource(planning:Planning, n:String) {

  //MultiKnapsack(items: Array[CBLSIntVar], itemsizes: Array[CBLSIntVar], binsizes:Array[CBLSIntVar])


  /** these are the activities that you can use for ejecting one of the conflicting activities */
  override def baseActivityForEjection(t: Int): Iterable[Activity] = null



  var resourceUsage:List[(Activity,Int)] = null

  var activityBin:Array[CBLSIntVar] = null;
  var activitySize:Array[CBLSIntVar] = null;


  case class ResourceAtTime(t:Int,
                            zeroBin:Bin,
                            bins:List[Bin],
                            var overallViolation:CBLSIntVar = null,
                            var violationNotZero:CBLSIntVar = null)

  private var binCount = 0;
  private def newBinNumber():Int = {
    val toReturn = binCount
    binCount += 1
    toReturn
  }

  val resourcesAtAllTimes:Array[ResourceAtTime] = Array.tabulate(planning.maxDuration)(t => {
    ResourceAtTime(t,
      Bin(newBinNumber(),0 /*maxSize*/),
      bins(t).map((binSize:Int) => Bin(newBinNumber(), binSize)),
      CBLSIntVar(planning.model, name="overall_violation") ,CBLSIntVar(planning.model, name="violationNotZero"))
  })

  def flatBins():Array[Bin] = {
    val allbins:List[Bin] = resourcesAtAllTimes.map(r => r.zeroBin :: r.bins).flatten.toList
    val binArray:Array[Bin] = Array.fill(binCount)(null)
    for(bin <- allbins){
      binArray(bin.number) = bin
    }
    binArray
  }

  def binSizes(bins:Array[Bin]):Array[Int] = {
    bins.map(bin => bin.size)
  }
  def binViolations(bins:Array[Bin]):Array[CBLSIntVar] = null

  /** This method builds a binpacking problem regrouping the items etc.
    * of a binpacking happending at the given point in time
    */
  def getBinPackingProblem(t:Int):BinPackingProblem = {
    null
  }



  /** This method is called by the framework before starting the scheduling
    * put anything that needs to be done after instantiation here
    */
  override def close(){
    val allBins = flatBins()


    val sc = ConstraintSystem(planning.model)
    val mkp = MultiKnapsack(activityBin, activitySize, binSizes(allBins).map((i:Int) => CBLSIntConst(i)))
    sc.post(mkp)

    for(bin <- allBins){
      bin.violation = mkp.violationOfBin(bin.number)
      bin.items = mkp.itemsInBin(bin.number)
    }

    for(r <- resourcesAtAllTimes){
      r.violationNotZero = Sum(r.bins.map(bin => bin.violation))
      r.overallViolation = r.violationNotZero + r.zeroBin.violation
    }
  }

  override def toAsciiArt(headerLength: Int): String = null

  /** you need to eject one of these to solve the conflict
    * this can be null if the problem is actually solved in between, or if the problem cannot be solved */
  override def conflictingActivities(t: Int): Iterable[Activity] = {
    //build the binPacking problem happening at time t
    val bp = getBinPackingProblem(t)

    BinPackingSolver.solveBinPacking(bp,MaxBPSteps)
    if(bp.overallViolation.Objective.value == 0) return null


    //try to solve it
    //if not working, identify which activity could be usefully ejected
   null
  }

  /** the first violation of the resource in time
    *
    * @return
    */
  override def worseOverShootTime: Int = 1

  override val overShoot: CBLSIntVar = null

}

/*
il faut aussi savoir dans quel ordre on va faire le flatten.
Je pense qu'il vaut mieux faire les cumulative en premier, puis les binPacking.
bon, au final on va quand-même les intertwiner alors je sais aps si ça va changer grand chose.
par contre, le pense qu'il vaut mieux faire du début à la fin pour les binPAcking parce-que le déplacement va induire plus de conflits que dans le cas de cumulatives.
 */

