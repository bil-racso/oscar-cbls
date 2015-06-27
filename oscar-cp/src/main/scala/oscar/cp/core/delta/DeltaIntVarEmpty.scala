package oscar.cp.core.delta

import oscar.cp.core.variables.CPIntVar

/** @author Renaud Hartert ren.hartert@gmail.com */
final class DeltaIntVarEmpty(final override val variable: CPIntVar) extends DeltaIntVar {
  final override val id: Int = 0
  final override val oldMin: Int = variable.min
  final override val oldMax: Int = variable.max
  final override val oldSize: Int = 1
  final override val changed: Boolean = false
  final override val size: Int = 0
  final override val values: Iterator[Int] = Iterator.empty
  final override def fillArray(values: Array[Int]): Int = 0
  final override val minChanged: Boolean = false
  final override val maxChanged: Boolean = false
  final override def update(): Unit = Unit
}
