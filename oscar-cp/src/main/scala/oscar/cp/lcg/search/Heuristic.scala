package oscar.cp.lcg.search

abstract class Heuristic {
  def decision: Function0[Unit]
}