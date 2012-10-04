package oscar.cp.mem.ParetoFront

import java.util.NoSuchElementException

class LinkedList {
	
	private var f : LinkedNode = null
	private var l : LinkedNode = null
	
	private var size = 0
	
	def isEmpty = size == 0
	
	def first = f
	def last = l
	
	def first_= (x : LinkedNode) { f = x }
	def last_= (x : LinkedNode) { l = x }
	
	def insert(value : Int) : LinkedNode = {
		
		val node = new LinkedNode(value, this)
		
		if (isEmpty) {
			f = node
			l = node
		}
		else insert0(f, node)
		
		size +=1		
		node		
	}
	
	def insert0(n : LinkedNode, node : LinkedNode) {
		
		if (n.isLast) {			
			n.next = node
			node.prev = n
		} 
		else if(n.value <= node.value) {
			
			node.next = n.next
			node.prev = n
			n.next = node
		}
		else insert0(n.next, node)
	}
	
	def remove(n : LinkedNode) {
		
		if (n.list != this)
			throw new RuntimeException("This node is not present in this list")		
		else if (isEmpty)
			throw new NoSuchElementException("This list is empty")
		else {
			if (n.isFirst) f = n.next else n.prev.next = n.next
			if (n.isLast)  l = n.prev else n.next.prev = n.prev
		}
		
		size -= 1
	}
	
	def clear() {
		size = 0
		f = null
		l = null
	}
}