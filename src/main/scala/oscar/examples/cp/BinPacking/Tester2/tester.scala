package oscar.examples.cp.BinPacking.Tester2

import oscar.examples.cp.BinPacking.Tester2.InstanceSolver
import oscar.examples.cp.BinPacking.DataStructures.BinPackingInstance

 
object tester {
  
  
	def main(args : Array[String]) {
	  println("tester valid solution" )
	  val generator = new BinPackingValidInstanceGenerator()
	  
 /*
	  val instance = new BinPackingInstance();
						
 instance.binCapacities = Array(0 to 0,6 to 7,0 to 0)
 instance.itemsSizes = Array(7)
 instance.binForItems = Array(
			Array(1,0))						
	  */
	  for(instance <- generator.generate.take(1000))
	  {
	    
	      println(instance.description())
	  

	    val resultShaw = (new InstanceSolver(instance)).solve(true,false,false)
	    val resultCurrentFlow = (new InstanceSolver(instance)).solve(true,true,false)
	    //val resultExtFlow = (new InstanceSolver(instance)).solve(true,false,true)
	    val results = resultShaw :: resultCurrentFlow :: Nil//resultExtFlow :: Nil
	    println( results .mkString(" "))
	    
	  }
	  
	  
	  
	}
}