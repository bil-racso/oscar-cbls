package oscar.invariants

abstract class SuspendableResult[+T] {
  def get(): T
}

case object Suspend extends SuspendableResult[Nothing] {
  def get = throw new NoSuchElementException("Suspend.get")
}
case class EndResult[T](v: T) extends SuspendableResult[T] {
  def get() = v
}
case object End extends SuspendableResult[Nothing] {
  def get = throw new NoSuchElementException("End.get")
  def apply[T](v: T) = new EndResult(v)
}