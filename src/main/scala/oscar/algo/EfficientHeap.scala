package oscar.algo

import java.util.NoSuchElementException

// If f is true, then a is an ancestor of b

class EfficientHeap[A : ClassManifest](maxSize : Int, f : (A, A) => Boolean) {
	
	private val heap : Array[A] = new Array(maxSize+1)
	
	private var heapSize = 0
	
	def head = heap(1)
	
	def size = heapSize
	
	def isEmpty = (size == 0)
	
	def clear() { heapSize = 0 }
	
	def enqueue(elem : A) {
		
		heapSize += 1
		
		var i = heapSize
		heap(i) = elem
		
		while (i > 1 && !f(heap(parent(i)), heap(i))) {
			
			val temp = heap(i)
			val p = parent(i)
			
			heap(i) = heap(p)
			heap(p) = temp
			i = p
		}
	}
	
	def dequeue() : A = {
		
		if (heapSize < 1)
			throw new NoSuchElementException
			
		val max : A = heap(1)
		heap(1) = heap(heapSize)
		heapSize -= 1
		
		heapify(1)
		
		return max
	}
	
	def heapify(j : Int) {
		
		var largest = j
		var i = 0
		
		do {	
			i = largest
			
			val l = left(i)
			val r = right(i)
			
			if (l <= heapSize && f(heap(l), heap(i)))
				largest = l
			else 
				largest = i
				
			if (r <= heapSize && f(heap(r), heap(largest)))
				largest = r
				
			if (largest != i) {
				val temp = heap(i)
				heap(i) = heap(largest)
				heap(largest) = temp
			}
			
		} while (largest != i)
	}
	
	def parent(i : Int) = i/2
	def left(i : Int) = 2*i
	def right(i : Int) = 2*i+1
}

object EfficientHeap {
	
	def main(args : Array[String]) {
		
		val h = new EfficientHeap[Int](10, (a, b) => a > b)
		h.enqueue(1)
		h.enqueue(8)
		h.enqueue(7)
		h.enqueue(3)
		h.enqueue(4)
		h.enqueue(6)
		h.enqueue(5)
		h.enqueue(2)
		
		while(!h.isEmpty) 
			println(h.dequeue)
	}
}