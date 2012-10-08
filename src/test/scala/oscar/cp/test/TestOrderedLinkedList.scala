package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalacheck._

import oscar.cp.mem.pareto.OrderedLinkedList
import oscar.cp.mem.pareto.LinkedNode


class TestOrderedLinkedList extends FunSuite with ShouldMatchers {

	test("Insert 1 : simple insert") {

		val list = new OrderedLinkedList
		
		val n1 = list insert 2
		val n2 = list insert 1
		val n3 = list insert 3
		
		list.first should be(n2)
		list.last should be(n3)
		
		list.size should be(3)
		
		list.first.next.next should be(list.last)
		list.last.prev.prev should be(list.first)
	}
	
	test("Insert 2 : same values") {
		
		val list = new OrderedLinkedList
		
		val n1 = list insert 5
		val n2 = list insert 5
		val n3 = list insert 5
		
		list.first.value should be(5)
		list.last.value should be(5)
		
		list.size should be(3)
		
		list.first.next.next should be(list.last)
		list.last.prev.prev should be(list.first)
	}
	
	test("Remove 1 : middle node") {
		
		val list = new OrderedLinkedList
		
		val n1 = list insert 1
		val n2 = list insert 2
		val n3 = list insert 3
		
		list remove n2
		
		list.first should be(n1)
		list.last should be(n3)
		
		list.size should be(2)
		
		list.first.next should be(list.last)
		list.last.prev should be(list.first)
	}
	
	test("Remove 2 : first node") {
		
		val list = new OrderedLinkedList
		
		val n1 = list insert 1
		val n2 = list insert 2
		val n3 = list insert 3
		
		list remove n1
		
		list.first should be(n2)
		list.last should be(n3)
		
		list.size should be(2)
		
		list.first.next should be(list.last)
		list.last.prev should be(list.first)
	}
	
	test("Remove 3 : last node") {
		
		val list = new OrderedLinkedList
		
		val n1 = list insert 1
		val n2 = list insert 2
		val n3 = list insert 3
		
		list remove n3
		
		list.first should be(n1)
		list.last should be(n2)
		
		list.size should be(2)
		
		list.first.next should be(list.last)
		list.last.prev should be(list.first)
	}
	
	test("Remove 4 : empty list") {
		
		val list = new OrderedLinkedList
		
		val n1 = list insert 1
		
		list remove n1
		
		// Exception 
		val thrown = intercept[NoSuchElementException] { list remove n1 }	
		thrown.getMessage should be("This list is empty")
		
		
		list.first should be(null)
		list.last should be(null)
		
		list.size should be(0)
	}
	
	test("Remove 5 : not in the list") {
		
		val list = new OrderedLinkedList
		val dummy = new OrderedLinkedList
		
		val n1 = dummy insert 1
		
		// Exception 
		val thrown = intercept[RuntimeException] { list remove n1 }	
		thrown.getMessage should be("This node is not present in this list")
		
		list.first should be(null)
		list.last should be(null)
		
		list.size should be(0)
		
		dummy.first should be(n1)
		dummy.last should be(n1)
		
		dummy.size should be(1)
	}
}