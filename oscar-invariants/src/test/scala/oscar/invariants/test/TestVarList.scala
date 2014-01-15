package oscar.invariants.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.collection.immutable._
import oscar.invariants._

class TestVarList extends FunSuite with ShouldMatchers {

  test("VarList[Int]") {

    val v = new VarList[Int]

    val v2 = v.mymap(_ * 2)

    var res = ""
    whenever(v2.isIncreased) { va =>
      res += va
    }

    v.add(1)
    v.add(5)
    v.add(9)

    res should equal("21018")

  }
  test("VarList[Int] - remove") {

    val v = new VarList[Int]

    val v2 = v.mymap(_ * 2)

    var res = ""
    whenever(v2.isIncreased) { va =>
      res += va
    }
    whenever(v2.isDecreased) { va =>
      res += -va
    }

    v.add(1)
    v.add(5)
    v.add(9)

    v.remove(5)

    res should equal("21018-10")

  }
  
  test("sum on VarList with mymap"){
    
    
    val v = new VarList[Int]

    val v2 = v.mymap(_ * 2)
    
    val s = sum(v2)
    
    v.add(6)
    v.add(10)
    v.remove(6)
    v.add(7)
    
    
    s() should be (34) 
    
  }
  
  test("mymap and VarInt - Sum"){
    case class Cont(i: IncrementalVar[Int])
    
    val list = new VarList[Cont]()
    val listRes = list.mymap(_.i)
    
    val s = sumVars(listRes)
    
    val v1 = new IncrementalVar(5)
    val v2 = new IncrementalVar(9)
    val v3 = new IncrementalVar(10)
    
    list.add(Cont(v1))
    
    s() should be (5)
    v1 := 7
    s() should be (7)
    
   list.add(Cont(v2))
   s() should be (16)
    
   v3 := 20
   s() should be (16)
   
    list.add(Cont(v3))
    s() should be (36)
    
    
    
  }
  

}