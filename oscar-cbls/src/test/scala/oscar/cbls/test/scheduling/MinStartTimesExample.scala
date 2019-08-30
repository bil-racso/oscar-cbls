package oscar.cbls.test.scheduling


import oscar.cbls.Store
import oscar.cbls.business.scheduling.model._
import oscar.cbls.business.scheduling.neighborhood._
import oscar.cbls.core.objective.Objective
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.core.objective.CascadingObjective
import oscar.cbls.lib.invariant.numeric.Sum

import scala.util.Random

object MinStartTimesExample extends App {
  // Random Generator
  val randomGen = new Random(10)

  val s = Store(checker = None, noCycle=false)

  //données
  val nAct = 10 //nbre d'activités
  val minDuration = 10  //nombre minimum de minutes pour une activité
  val maxDuration = 30  //nombre maximum de minutes pour une activité

  //créations des variables pour le modèle

  //1) durations(i) = durée de l'activité i
  val durations: Array[Long] = Array.tabulate(nAct)(_ => randomInterval(minDuration, maxDuration))

  for { i <- durations.indices} { println(s"Duration ($i) = ${durations(i)}") }

  //2) les contraintes de précédences (on peut mettre une liste vide)
  val precPairs = List((0, 3), (2, 1)) //(a,b) l'activité numéro 0 doit se dérouler avant l'activité numéro 3

  //3) Activités initiales ???
  val initialActs = 0 until nAct //nAct on les met toutes ??

  //4) Temps de départ mimimum pour les tâches
  val minStarts = Map(0 -> 30L)

  //5) Création des resources

  val resource1 = new DisjunctiveResource(List(0,2,5,7,8,9))
  val resource2 = new CumulativeResource(5L,Map(1-> 2L , 3 -> 1L , 4 -> 1L , 6 -> 1L , 7 -> 1L))

  val resources = Array[ResourceConstraint](resource1,resource2)

  //la contrainte de scheduling
  val scheduling = new Schedule(s, durations, precPairs, minStarts, initialActs, resources)

  //début des activités
  val minBegin = 30
  val maxBegin = 200
  //val timeActBegin = Array.tabulate(nAct)( _ => randomInterval(minBegin, maxBegin))

  val timeActBegin = Array.tabulate(nAct)( _ => 0)
  timeActBegin(0) = 30

  //contraintes sur le début des activités ?
  //val constraintTimeActBegin = Array.tabulate(nAct)(i =>
  //  scheduling.startTimes(i) ge timeActBegin(i))

  //val constraintSystem = new ConstraintSystem(s)
  //constraintTimeActBegin.foreach(constraintSystem.post(_))

  //val objFunc = new CascadingObjective(constraintSystem,scheduling.makeSpan)
  val objFunc = new CascadingObjective(scheduling.makeSpan, Sum(scheduling.startTimes))

  //la fonction objective
  //val objFunc = Objective(scheduling.makeSpan)

  //fermer le modèle
  s.close()

  //les voisinnages d'exploration
  /*
   * Liste des voisinnages
   * - AddActivity
   * - ReinsertActivity
   * - RemoveActivity
   * - ReplaceActivity
   * - SchedulingMove
   * - SwapActivity
   */

  val swapNH = new SwapActivity(scheduling, "Swap")
  val reinsertNH = new ReinsertActivity(scheduling, "Reinsert")
  val addNH = new AddActivity(scheduling, "Add")
  val removeNH = new RemoveActivity(scheduling, "Remove")
  val replaceNHcomb = removeNH andThen addNH
  //val replaceNH = new ReplaceActivity(scheduling, "Replace")
  val combinedNH = BestSlopeFirst(List(Profile(reinsertNH), Profile(swapNH), Profile(replaceNHcomb))) //, Profile(replaceNH)
  //Profile c'est pour les stats : à enlever

  //lancement de la recherche
  combinedNH.verbose = 1
  combinedNH.doAllMoves(obj = objFunc)


  println(combinedNH.profilingStatistics)
  println(s"Activities = ${scheduling.activitiesPriorList.value.toList}")

  println("------------------------")

  println("Scheduling start times = [  ")
  scheduling.startTimes.foreach(v => println(s"    $v"))
  println("]")

  println(s"Objective Function = $objFunc")

  println(resource2.activitiesConsumption) //mapping
  println(resource2.usingActivities)

  //println(constraintSystem.violation)

  /**
   * Rend un nombre aléatoire entre inf et sup
   */

  def randomInterval(inf: Long, sup: Long): Long = {
    require(inf <= sup)
    val rdVal = if (inf == sup) 0L else Math.abs(randomGen.nextLong()) % (sup - inf)
    inf + rdVal
  }
}
