package oscar.cbls.scheduling.model

import oscar.cbls.invariants.core.computation.CBLSIntVar

/**
 * A bin packing resource is a resource that is held only at the first time unit of the activity using it
 * it is used for a certain amount, which is activity-dependent, and supposed constant
 * its specificity is that each time unit has a number of bins, and the activities starting at the same time unit
 * must fit into the bins of the time unit.
 *
 * We suppose that the bins cover the full history that is available in the planning.
 *
 */
class BinPackingResource(planning:Planning, n:String, bins:Int => Iterable[Int]) extends  Resource(planning:Planning, n:String) {

  //création des bins et des clusters par jours
  //on fait un array d'array de bins

  val binCLusters:Array[Array[CBLSIntVar]] = Array.tabulate(planning.maxduration)((t:Int) => {
    val binsT:Iterable[Int] = bins(t)
    val vars = Array.fill(binsT.size)(((size:Int) => ))
  null
  })


  //pour chaque unité de temps, on crée une ausse bin, qui conteint ce qu'on apporte comme nouvel item
  //on veut:
  //flatbins
  //flatBinSizes
  //t => (usage(Activity,UsageLevel,BinNumber,ItemViolation),bins:List[(size,maxSize)],)
  //

  //MultiKnapsack(items: Array[CBLSIntVar], itemsizes: Array[CBLSIntVar], binsizes:Array[CBLSIntVar])


  override def toAsciiArt(headerLength: Int): String = ???

  /** you need to eject one of these to solve the conflict
    * this can be null if the problem is actually solved in between, or if the problem cannot be solved */
  override def conflictingActivities(t: Int): Iterable[Activity] = ???

  /** the first violation of the resource in time
    *
    * @return
    */
  override def worseOverShootTime: Int = ???


  /** this method is called by the framework before starting the scheduling
    * put anything that needs to be done after instantiation here
    */
  override def close(): Unit = ???

  override val overShoot: CBLSIntVar = _

}
