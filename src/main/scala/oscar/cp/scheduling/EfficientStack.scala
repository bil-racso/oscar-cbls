package oscar.cp.scheduling

import java.util.NoSuchElementException
import JSci.maths.statistics.OutOfRangeException

class EfficientStack[A : ClassManifest](maxSize : Int) {
	
	private val stack : Array[A] = new Array(maxSize)
	
	private var stackSize = 0
	private var t = 0
	
	def top = stack(t)
	def size = stackSize
	def isEmpty = (stackSize == 0)
	
	def clear() = { 
		t = 0
		stackSize = 0
	}
	
	def enqueue(elem : A) {
		
		if (stackSize == maxSize) {
			throw new OutOfRangeException("the stack is full")
		}
		
		stack(t) = elem
		
		stackSize +=1
		t = t + 1
	}
	
	def dequeue() : A = {
		
		if (stackSize == 0) {
			throw new NoSuchElementException
		} else {
			
			stackSize -= 1
			t = t - 1
			
			val elem = stack(t)
				
			return elem
		}		
	}
}

object EfficientStack {
	
	def main(args : Array[String]) {
		
		val q = new EfficientStack[Int](5)
		
		q.enqueue(8)
		q.enqueue(7)
		q.enqueue(3)
		q.enqueue(2)
		q.enqueue(1)
		println(q.dequeue())
		println(q.dequeue())
		println(q.dequeue())
		q.enqueue(4)
		println(q.dequeue())
		q.enqueue(6)
		q.enqueue(5)
		println(q.dequeue())
		println(q.dequeue())
		println(q.dequeue())
		println(q.dequeue())
		println(q.isEmpty)
	}
}