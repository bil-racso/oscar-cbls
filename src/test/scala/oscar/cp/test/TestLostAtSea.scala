package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._
import oscar.cp.search._
import oscar.cp.modeling._

import org.scalacheck._

class TestLostAtSea extends FunSuite with ShouldMatchers with CPModel {

        val proba = Array(Array(3,0,0,3,2,4,2,3),
                        Array(3,3,3,1,2,4,1,4),
                        Array(0,4,0,1,2,3,4,0),
                        Array(1,1,0,3,4,1,1,0),
                        Array(1,1,3,3,1,2,2,4),
                        Array(0,2,3,3,3,0,2,4),
                        Array(2,3,2,4,2,4,1,1),
                        Array(2,1,2,2,2,4,1,3))
       def getLineCol(i: Int) = (i/8,i%8)
                        
       def neighbors(i: Int) = {
        val (l,c) = getLineCol(i)
        def toInt(lc: (Int,Int)) = lc._1*8 + lc._2
        Set((l+1,c),(l-1,c),(l,c+1),(l,c-1)).filter{ case(l,c) => (l >= 0 && l < 8 && c >= 0 && c < 8)}.map(toInt(_))
       } 
        

  test("Table Model") {
       // set of valid transitions pair
       val tuples = (for (i <- 0 until 64; j <- neighbors(i)) yield (i,j)).toSet 
       
       val cp = CPSolver()
       
       var best = 1000
             
       val path = Array.fill(10)(CPVarInt(cp,0 until 64))
       
       val sol = Array.fill(10)(0) 
       
       val obj = sum(0 until 10)(i => element(proba.flatten,path(i)))
       
       cp.maximize(obj) subjectTo {
                for (i <- 0 until 9) {
                  cp.add(table(path(i),path(i+1),tuples)) // for each consecutive visits, give the possible valid transitions
                }
                cp.add(alldifferent(path),Strong) // each visit must be different
       } exploration {
         cp.binaryFirstFail(path)
         (0 until 10).foreach(i => sol(i) = path(i).getValue()) // record the solution
         best = obj.getValue()
       }
       best should be(33)
  }  
  
  test("Circuit Model") {
    
    
       val cp = CPSolver()
       
       val succ = Array.tabulate(64)(i => CPVarInt(cp,neighbors(i)))
       
       val path = Array.fill(10)(CPVarInt(cp,0 until 64)) // represent the path of length ten which is the solution
       
       val sol = Array.fill(10)(0) 
       
       var best = 1000
       
       val obj = sum(0 until 10)(i => element(proba.flatten,path(i)))
       
       cp.maximize(obj) subjectTo {
                
    	  		for (i <- 0 until 9) {
                  cp.add(element(succ,path(i),path(i+1))) 
                }
                cp.add(circuit(succ),Strong)
       } exploration {
         cp.binaryFirstFail(path)
         (0 until 10).foreach(i => sol(i) = path(i).getValue()) // record the solution
         best = obj.getValue()
       }
       best should be(33)

  } 

}
