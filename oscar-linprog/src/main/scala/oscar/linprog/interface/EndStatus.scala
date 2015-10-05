package oscar.linprog.interface

sealed abstract class EndStatus(val name: String) {
  override def toString: String = name
}

case object NOT_SOLVED extends EndStatus("NOT_SOLVED")
case object OPTIMAL extends EndStatus("OPTIMAL")
case object SUBOPTIMAL extends EndStatus("SUBOPTIMAL")
case object UNBOUNDED extends EndStatus("UNBOUNDED")
case object INFEASIBLE extends EndStatus("INFEASIBLE")
case object NO_SOLUTION extends EndStatus("NO_SOLUTION")
