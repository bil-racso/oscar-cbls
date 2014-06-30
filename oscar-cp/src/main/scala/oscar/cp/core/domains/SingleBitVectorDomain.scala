package oscar.cp.core.domains

import oscar.algo.reversible.ReversibleContext
import oscar.algo.reversible.ReversibleInt
import scala.util.Random
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._

// Still contains some bugs to fix
class SingleBitVectorDomain(context: ReversibleContext, val minValue: Int, val maxValue: Int) extends SparseIntDomain {
 
  // Initial size of the domain
  private val maxBits = maxValue - minValue + 1
  
  // Consistency check
  require(maxBits <= 32, "the set cannot contain more than 32 elements")
  
  // Domain representation
  private val bits = new ReversibleInt(context, (1 << maxBits) - 1)
  private val nBits = new ReversibleInt(context, maxBits)
  private val minId = new ReversibleInt(context, 0)
  private val maxId = new ReversibleInt(context, maxBits - 1)

  override def size: Int = nBits.value

  override def isEmpty: Boolean = nBits.value == 0
  
  override def isBound: Boolean = nBits.value == 1

  override def max: Int = {
    updateMaxId()
    minValue + maxId.value
  }

  override def min: Int = {
    updateMinId()
    minValue + minId.value
  }
  
  override def randomValue(rand: Random): Int = {
    val n = nBits.value
    val r = rand.nextInt(n) + 1
    val minVal = min // Lazy update
    // Select the r integer
    if (r == 1) minVal
    else if (r == n) max // Lazy update
    else {
      val domain = bits.value
      var counter = 1
      var id = minId.value + 1
      while (counter < r) {
        if ((domain & (1 << id)) == 1) counter += 1
        id += 1
      }
      minValue + id
    }
  }
  
  def hasValue(value: Int): Boolean = {
    val bit = 1 << (value - minValue)
    (bits.value & bit) == bit
  }
  
  def removeValue(value: Int): CPOutcome = {
    val bit = 1 << (value - minValue)
    val domain = bits.value
    if ((domain & bit) == 0) Suspend
    else {
      bits.value = domain ^ bit
      nBits.decr
      Suspend
    }
  }
  
  def assign(value: Int): CPOutcome = {
    val id = (value - minValue)
    val bit = 1 << id
    if ((bits.value & bit) == 0) Failure
    else {
      bits.value = bit
      nBits.value = 1
      Suspend
    }
  }
  
  @inline
  private def updateMinId(): Unit = {
    var id = minId.value
    val max = maxId.value
    val domain = bits.value
    while (id < max && (domain & (1 << id)) == 0) {
      id += 1
    }
    minId.value = id
  }
  
  @inline
  private def updateMaxId(): Unit = {
    var id = maxId.value
    val min = minId.value
    val domain = bits.value
    while (id > min && (domain & (1 << id)) == 0) {
      id -= 1
    }
    maxId.value = id
  }

  def updateMin(value: Int): CPOutcome = {
    val id = value - minValue
    updateMinId()
    val minBitId = minId.value
    if (id <= minBitId) Suspend
    else {
      updateMaxId()
      val maxBitId = maxId.value
      if (id > maxBitId) {
        nBits.value = 0
        Failure
      }
      else if (id == maxBitId) assign(value) 
      else {
        var i = minBitId
        var domain = bits.value
        var n = nBits.value
        while (i < id) {
          val bit = (1 << i)
          if ((domain & bit) == bit) {
            domain = domain ^ bit
            n -= 1
          } 
          i += 1
        }
        bits.value = domain
        nBits.value = n
        minId.value = id
        Suspend
      }
    }
  }
  

  def updateMax(value: Int): CPOutcome = {
    
    val id = value - minValue
    updateMaxId()
    val maxBitId = maxId.value
    if (id >= maxBitId) Suspend
    else {
      updateMinId()
      val minBitId = minId.value
      if (id < minBitId) {
        nBits.value = 0
        Failure
      }
      else if (id == minBitId) assign(value)
      else {
        var i = maxBitId
        var domain = bits.value
        var n = nBits.value
        while (i > id) {
          val bit = (1 << i)
          if ((domain & bit) == bit) {
            domain = domain ^ bit
            n -= 1
          } 
          i -= 1
        }
        bits.value = domain
        nBits.value = n
        maxId.value = id
        Suspend
      }
    }
  }

  def nextValue(value: Int): Int = {
    updateMaxId()
    val max = maxId.value
    var id = value - minValue + 1
    if (id == max) value - 1
    else {
      val domain = bits.value
      while ((domain & (1 << id)) == 0) id += 1
      minValue + id
    }
  }

  def prevValue(value: Int): Int = {
    updateMinId()
    val min = minId.value
    var id = value - minValue + 1
    if (id == min) value + 1
    else {
      val domain = bits.value
      while ((domain & (1 << id)) == 0) id -= 1
      minValue + id
    }
  }
  
  override def iterator: Iterator[Int] = {
    updateMinId()
    updateMaxId()
    new Iterator[Int] {      
      private var id = minId.value
      private val max = maxId.value
      private val domain = bits.value   
      def next(): Int = {
        val value = minValue + id
        searchNext()
        value
      }
      def hasNext(): Boolean = id <= max
      private def searchNext(): Unit = {
        id += 1
        while (id <= max && ((domain & (1 << id)) == 0)) {
          id += 1
        }
      }
    }
  }

  // Not implemented
  def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = ???
}