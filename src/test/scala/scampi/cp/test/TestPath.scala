package scampi.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import scampi.cp.constraints._
import scampi.cp.core._
import scampi.cp.search._
import scampi.cp.modeling._

import org.scalacheck._

class TestPath extends FunSuite with ShouldMatchers with CPModel {

  val succ = Array(Set(0,1),Set(1,2,3),Set(2,4),Set(3,2,5),Set(4,5),Set(5,0))
  
  // 0 --------> 1 ------>2
  //               \    /\ \
  //               _\/ /   _\/
  //                 3       4
  //                  \     /
  //                   \   /
  //                   _\\/_
  //                     5
  test("Path1") {
      
	  val cp = CPSolver()
	  var X = Array.tabulate(succ.length)(i => CPVarInt(cp,succ(i)))
	  var start = CPVarInt(cp, 0)
	  var end = CPVarInt(cp, 5)
	  var length = CPVarInt(cp, 0 to 5)
	  var nbSol = 0
	  cp.solveAll subjectTo {
        cp.add(path(X,start,end,length))
      } exploration {
        cp.binary(X)
        nbSol += 1
        
      }
      nbSol should be(3)
  }  
  
  test("Path2") {
      
	  val cp = CPSolver()
	  var X = Array.tabulate(succ.length)(i => CPVarInt(cp,succ(i)))
	  var start = CPVarInt(cp, 0)
	  var end = CPVarInt(cp, 5)
	  var length = CPVarInt(cp, 0 to 5)
	  var nbSol = 0
	  cp.solveAll subjectTo {
        cp.add(path(X,start,end,length))
        cp.add(length > 3)
      } exploration {
        cp.binary(X)
        println(X.mkString(","))
        nbSol += 1
      }
      nbSol should be(2)
  }   
  


}