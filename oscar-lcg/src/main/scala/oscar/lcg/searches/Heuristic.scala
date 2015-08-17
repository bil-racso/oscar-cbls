package oscar.lcg.searches

import oscar.lcg.core.Literal

abstract class Heuristic {
  def decision(): Literal
}