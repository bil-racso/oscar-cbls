package oscar.algebra.test

import oscar.algebra.Var

case class TestVar(name: String, initValue: Double) extends Var {
  private var _value: Double = initValue

  override def value: Option[Double] = Some(_value)
  def value_=(d: Double) = _value = d
}
