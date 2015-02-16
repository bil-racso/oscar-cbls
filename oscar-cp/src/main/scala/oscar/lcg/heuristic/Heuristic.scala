package oscar.lcg.heuristic

abstract class Heuristic {
  def decision: Function0[Unit]
}