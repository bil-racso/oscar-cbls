package oscar.lcg.heuristic

/** @author Renaud Hartert ren.hartert@gmail.com */
abstract class Heuristic {
  def decision: Function0[Unit]
}