package oscar.examples.cp.memScheduling
import oscar.cp.memScheduling._
import scala.Array.canBuildFrom

object HybridReentrantShopScheduling extends App with Scheduler with Reader {
  
  read fromFile "data/memScheduling/HybridReentrantSSInstance/hrs-10_1_1.txt"
  val Array(nbJobs, nbMachines) = read fileFor 2 int
  val firstOpTime		= read fileFor 1 fillerForArrayOf nbJobs int
  val secondOpTime	= read fileFor nbJobs int
  val thirdOpTime 	= read fileFor nbJobs int
  
  setHorizonTo(firstOpTime.sum + secondOpTime.sum + thirdOpTime.sum)

  println(nbJobs)
  println(nbMachines)
  firstOpTime.map(println)
  secondOpTime.map(println)
  thirdOpTime.map(println)
  
}