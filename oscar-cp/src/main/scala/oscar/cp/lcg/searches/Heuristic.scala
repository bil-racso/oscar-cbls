package oscar.cp.lcg.searches

abstract class Heuristic {
  def decision: Function0[Unit]
}