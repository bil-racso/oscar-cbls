package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.cp.constraints._
import oscar.cp.core._
import oscar.cp.search._
import oscar.cp.modeling._
import oscar.cp.scheduling._

class TestPartialOrderSchedule extends FunSuite with ShouldMatchers with CPModel {

	test("Test : Precedences generation") {
		
		val a = new FixedActivity(0, 0, 3, 3, 0)
		val b = new FixedActivity(1, 0, 2, 1, 0)
		val c = new FixedActivity(2, 2, 3, 2, 0)
		val d = new FixedActivity(3, 3, 6, 4, 0)
		val e = new FixedActivity(4, 5, 10, 1, 0)
		val f = new FixedActivity(5, 6, 7, 3, 0)
		val g = new FixedActivity(6, 7, 9, 2, 0)
		val h = new FixedActivity(7, 9, 10, 4, 0)
		val i = new FixedActivity(8, 10, 12, 1, 0)
		
		val tasks = Array(a, b, c, d, e, f, g, h, i)
		
		val capacity = 5
		
		val precedencesSet : Set[Tuple2[Int,Int]] = Set((0, 3), // A -> D
														(1, 2), // B -> C
														(2, 3), // C -> D
														(2, 4), // C -> E
														(3, 5), // D -> F
														(3, 7), // D -> H
														(4, 8), // E -> I
														(5, 7), // F -> H
														(5, 6), // F -> G
														(6, 7), // G -> H
														(7, 8)) // H -> I
		
		val precedences = PartialOrderSchedule.getPrecedences(tasks, Array(capacity))
		
		for(p <- precedences) 
			precedencesSet.contains(p) should be(true)
	}
}