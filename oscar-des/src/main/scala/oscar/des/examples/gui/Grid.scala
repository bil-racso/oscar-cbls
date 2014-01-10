package oscar.des.engine.examples.gui

class Grid[A](val height: Int, val width: Int) extends Function2[Int,Int, A] with Iterable[A] {
  
  private val delegate = scala.collection.mutable.Map[(Int,Int),A]()
  
  override def apply(row: Int, col: Int): A = delegate(row,col)
  
  def update(row: Int, col: Int, elem: A) { delegate.put((row,col),elem)}
  
  override def size: Int = height * width
  
  def iterator: Iterator[A] = delegate.toList.sortBy(_._1).map(_._2).iterator
  
}
