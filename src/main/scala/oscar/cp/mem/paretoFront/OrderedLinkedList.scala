package oscar.cp.mem.paretoFront

import java.util.NoSuchElementException

class OrderedLinkedList {
	
	private var fNode : LinkedNode = null
	private var lNode : LinkedNode = null
	
	private var s = 0
	
	def size = s
	def isEmpty = s == 0
	
	def first = fNode
	def last = lNode
	
	def first_= (x : LinkedNode) { fNode = x }
	def last_= (x : LinkedNode) { lNode = x }
	
	def insert(value : Int) : LinkedNode = {
		
		val node = new LinkedNode(value, this)
		
		if (isEmpty) {
			first = node
			last = node
		}
		else insert0(first, node)
		
		s +=1		
		node		
	}
	
	def insert0(n : LinkedNode, newNode : LinkedNode) {
		
		if(n.value > newNode.value) {
			
			if (n.isFirst) {
				
				first = newNode
				n.prev = newNode
				newNode.next = n	
			} 
			else {
				newNode.prev = n.prev
				newNode.prev.next = newNode
				
				newNode.next = n
				n.prev = newNode
			}
		}
		else if (n.isLast) {
			
			last = newNode
			newNode.prev = n
			n.next = newNode
		} 
		else insert0(n.next, newNode)
	}
	
	def remove(n : LinkedNode) {
		
		if (n.list != this)
			throw new RuntimeException("This node is not present in this list")		
		else if (isEmpty)
			throw new NoSuchElementException("This list is empty")
		else {
			if (n.isFirst) first = n.next else n.prev.next = n.next
			if (n.isLast) last = n.prev else n.next.prev = n.prev
		}
		
		s -= 1
	}
	
	def clear() {
		s = 0
		first = null
		last = null
	}
}