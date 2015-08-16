package oscar.lcg.search

import oscar.lcg.core.Literal

abstract class Heuristic {
  def decision(): Literal
}