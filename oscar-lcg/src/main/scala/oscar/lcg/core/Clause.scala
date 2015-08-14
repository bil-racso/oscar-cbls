package oscar.lcg.core

abstract class Clause {
  def propagate(): Boolean
}