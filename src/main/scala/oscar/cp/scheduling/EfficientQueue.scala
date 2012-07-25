package oscar.cp.scheduling

import java.util.NoSuchElementException

class EfficientQueue[A : ClassManifest](maxSize : Int) {
	
	private val queue : Array[A] = new Array(maxSize)
	
	private var queueSize = 0
	private var f = -1
	private var l = 0
	
	def first = queue(f)
	def last  = queue(l)
	def size = queueSize
	def isEmpty = (queueSize == 0)
	
	def clear() = { 
		f = 0
		l = 0
		queueSize = 0
	}
	
	def enqueue(elem : A) {
		
		if (queueSize == maxSize) {
			throw new Exception("the queue is full")
		}
		
		queue(l) = elem
		
		queueSize +=1
		
		l = l + 1
		if (l == maxSize) l = 0
	}
	
	def dequeue() : A = {
		
		if (queueSize == 0) {
			throw new NoSuchElementException
		} else {
			
			val elem = queue(f)
			
			queueSize -= 1
			
			f = f + 1
			if (f == maxSize) f = 0
				
			return elem
		}		
	}
}

object EfficientQueue {
	
	def main(args : Array[String]) {
		
		val q = new EfficientQueue[Int](5)
		
		q.enqueue(1)
		q.enqueue(2)
		q.enqueue(3)
		q.enqueue(4)
		q.enqueue(5)
		println(q.dequeue())
		println(q.dequeue())
		println(q.dequeue())
		q.enqueue(6)
		println(q.dequeue())
		q.enqueue(7)
		q.enqueue(8)
		println(q.dequeue())
		println(q.dequeue())
		println(q.dequeue())
		println(q.dequeue())
		println(q.isEmpty)
	}
}