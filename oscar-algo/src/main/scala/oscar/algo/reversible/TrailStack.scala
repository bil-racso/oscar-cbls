package oscar.algo.reversible

abstract class TrailEntry { def restore(): Unit }

class TrailEntryImpl[@specialized T](reversible: Reversible[T], value: T) extends TrailEntry {
  override def restore(): Unit = reversible.restore(value)
}

/** 
 *  An array-based stack for `TrailEntry`
 *  
 *  @author Renaud Hartert ren.hartert@gmail.com 
 */
class TrailStack(initialSize: Int = 100) {
  
  private var stack: Array[TrailEntry] = Array.ofDim(initialSize)
  private var index: Int = 0

  /**
   *  Return the size of the stack
   *  
   *  @return The size of the stack
   */
  def size: Int = index

  /**
   *  Test if the stack is empty or not
   *  
   *  @return `true` if the stack is empty, `false` otherwise
   */
  def isEmpty = index == 0

  /**
   *  Return the top element of the stack without removing it
   *  
   *  Throws an exception if the stack is empty
   *  
   *  @return The top element of the stack
   */
  def top: TrailEntry = stack(index - 1)
  
  /**
   *  Return the last element of the stack
   *  
   *  Throws an exception if the stack is empty
   *  
   *  @return The last element of the stack
   */
  def last: TrailEntry = {
    if (index == 0) sys.error("Stack empty")
    else stack(0)
  }

  /** 
   *  Push an element onto the stack.
   *
   *  @param entry The element to push
   */
  def push(entry: TrailEntry): Unit = {
    if (index == stack.length) growStack()   
    stack(index) = entry
    index += 1
  }
  
  /** 
   *  Pop the element on top of the stack
   *  
   *  @return The element on top of the stack
   */
  def pop(): TrailEntry = {
    if (index == 0) sys.error("Stack empty")
    index -= 1
    stack(index)
  }
  
  // Double the size of the stack
  @inline private def growStack(): Unit = {
    val newStack = Array.ofDim[TrailEntry](stack.length * 2)
    System.arraycopy(stack, 0, newStack, 0, stack.length)
    stack = newStack
  }
}