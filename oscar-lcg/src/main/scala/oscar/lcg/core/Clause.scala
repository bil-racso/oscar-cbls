package oscar.lcg.core

abstract class Clause {
  def setup(): Boolean
  def propagate(literal: Literal): Boolean
}