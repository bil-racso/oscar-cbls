package oscar.reversible

class ReversibleInterval(s: ReversibleSearchNode, val minValue: Int, val maxValue: Int) {
  
  private val _min = new ReversibleInt(s, minValue)

  private val _max = new ReversibleInt(s, maxValue)

  private val _size = new ReversibleInt(s, maxValue - minValue + 1)
  
  def size = 0 max _size.value
  
  def min: Int = {
	assert(!isEmpty)
    _min.value
  }
  
  def max: Int = {
    assert(!isEmpty)
    _max.value
  }
  

  def isEmpty = size <= 0
  
  /**
   * @param value
   * @return smallest value in the domain >= value, value-1 is returned if no such value
   */ 
  def nextValue(value: Int): Int = {
    if (isEmpty || value > max) {
      value-1
    } else if (value < min) {
      return min
    } else value
  }
  
  /**
   * @param value
   * @return largest value in the domain <= value, value+1 is returned if no such value
   */ 
  def prevValue(value: Int): Int = {
    if (isEmpty || value < min) {
      value+1
    } else if (value > max) {
      return max
    } else value
  }
  
  def removeValue(value: Int) {
    if (value == min) updateMin(value+1)
    if (value == max) updateMax(value-1)
  }
  
  def hasValue(value: Int) = !isEmpty && value <= max && value >= min
  
  def iterator: Iterator[Int] = {
    (min to max).iterator
  }
  
  def updateMax(value: Int) {
    if (value >= max) return
    else if (value < min) _size.value = 0
    else {
      _size.value = value-min+1
      _max.value = value
    }
  }
  
  def updateMin(value: Int) {
    if (value <= min) return
    else if (value > max) _size.value = 0
    else {
      _size.value = max-value+1
      _min.value = value
    }    
    
  }
  
  def assign(value: Int) {
      updateMax(value)
      if (!isEmpty) updateMin(value)
  }

}