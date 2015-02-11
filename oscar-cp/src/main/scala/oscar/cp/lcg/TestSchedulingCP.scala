package oscar.cp.lcg
  
import oscar.cp._
import oscar.cp.lcg.searches.MinMinHeuristic
import oscar.cp.lcg.searches.LCGSearch
import oscar.cp.lcg.constraints.DecompCheckerCP
  
object TestSchedulingCP extends App {
  
  // Data
  val instance = Array((5, 1), (3, 1), (9, 3), (1, 2), (2, 2), (8, 1), (3, 2), (2, 2), (2, 1), (1, 1), (1, 2), (2, 2), (8, 1))
  val durations = instance.map(_._1)
  val demands = instance.map(_._2)
  val horizon = 19//durations.sum
  val nTasks = durations.length
  val capa = 4
  
  implicit val cpSolver: CPSolver = CPSolver() 
  
  // Decision variables
  val starts = Array.tabulate(nTasks)(t => CPIntVar(0, horizon - durations(t), "Task_" + (t + 1)))
  
  // LCG Constraints
  val constraint = new DecompCheckerCP(starts, durations, demands, capa, horizon)
  cpSolver.add(constraint)
  
  search(binaryStatic(starts))
  
  onSolution {
    println("\nSOLUTION\n" + starts.map(v => v.name + " = " + v.min).mkString("\n"))
  }
  
  val stats = start(nSols = 1)
  println(stats)
}