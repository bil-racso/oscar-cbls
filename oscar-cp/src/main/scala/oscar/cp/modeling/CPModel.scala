package oscar.cp.modeling

trait CPModel {
  implicit val solver: CPSolver = CPSolver()
}
