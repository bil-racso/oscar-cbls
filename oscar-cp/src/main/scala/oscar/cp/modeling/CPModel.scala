package oscar.cp.modeling

trait CPModel extends App {
  implicit val solver: CPSolver = CPSolver()
}