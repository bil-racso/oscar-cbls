package oscar.sat.core

class Literal(final val varId: Int, final val signed: Boolean, op: Literal, name: String) {
  def this (varId: Int, name: String) = this(varId, false, null, name)
  final val id: Int = if (signed) varId * 2 + 1 else varId * 2
  final val opposite: Literal = if (op == null) new Literal(varId, true, this, s"-$name") else op
  final def unary_-(): Literal = opposite
  final override def toString: String = name
}
