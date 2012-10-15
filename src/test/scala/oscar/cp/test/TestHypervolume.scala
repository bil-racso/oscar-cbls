package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalacheck._

import oscar.cp.mem.Hypervolume

class TestHypervolume extends FunSuite with ShouldMatchers {
	
	trait Points1 {
			
		val ref = Array(0.0, 0.0)
			
		val p1  = Array(0.5, 0.5)
		val p2  = Array(0.25, 0.75)
		val p3  = Array(0.75, 0.25)
	}
	
	trait Points2 {
			
		val ref = Array(0.5, 0.5)
			
		val p1  = Array(1.0, 1.0)
		val p2  = Array(0.75, 1.25)
		val p3  = Array(1.25, 0.75)
	}
	
	trait Points3 {
			
		val ref = Array(0.5, 0.5)
			
		val p1  = Array(1.0, 1.0, 3.0)
	}

	
	test("simple2DHypervolume 1 : one point") {

		new Points1 {
			
			val ps = Array(p1)			
			val vol = Hypervolume.simple2DHypervolume(ps, ref)
			
			vol should be(0.25)
		}
	}
	
	test("simple2DHypervolume 2 : one point") {

		new Points1 {
			
			val ps = Array(p2)			
			val vol = Hypervolume.simple2DHypervolume(ps, ref)
			
			vol should be(0.1875)
		}
	}
	
	test("simple2DHypervolume 3 : one point") {

		new Points1 {
			
			val ps = Array(p3)			
			val vol = Hypervolume.simple2DHypervolume(ps, ref)
			
			vol should be(0.1875)
		}
	}
	
	test("simple2DHypervolume 4 : three points") {

		new Points1 {
			
			val ps = Array(p1, p2, p3)			
			val vol = Hypervolume.simple2DHypervolume(ps, ref)
			
			vol should be(0.375)
		}
	}	
	
	test("simple2DHypervolume 5 : three points") {

		new Points2 {
			
			val ps = Array(p1, p2, p3)			
			val vol = Hypervolume.simple2DHypervolume(ps, ref)
			
			vol should be(0.375)
		}
	}	
	
	test("simple2DHypervolume 6 : illegal dimensions") {

		new Points3 {
			
			val ps = Array(p1)			
			
			val thrown = intercept[IllegalArgumentException] { Hypervolume.simple2DHypervolume(ps, ref) }	
			thrown.getMessage should be("This algorithm only works with two dimensional points.")
		}
	}	
	
	test("simple2DHypervolume 7 : empty set") {

		new Points1 {
			
			val ps : Array[Array[Double]] = Array()			
			val vol = Hypervolume.simple2DHypervolume(ps, ref)
			
			vol should be(0.0)
		}
	}
}
