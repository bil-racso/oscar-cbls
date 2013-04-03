package oscar.examples.cp.BinPacking.Tester2

import oscar.examples.cp.BinPacking.Tester2.InstanceSolver
import oscar.examples.cp.BinPacking.DataStructures.BinPackingInstance

 
class Results {
  var results = Array[(Int,Long,Int)]()
  var nbResults = 0
  def addResult(result : Array[(Int,Long,Int)])
  {
    
    if (nbResults == 0)
      results = result
    for ((r:(Int,Long,Int), i:Int) <- result.zipWithIndex)
      results(i) = (results(i)._1 + r._1,results(i)._2 + r._2,results(i)._3 + r._3)
    nbResults += 1
  }
  
  def total() = {
    for (r:(Int,Long,Int) <- results)
      yield ((r._1 +.0)/nbResults, (r._2 +.0)/nbResults, (r._3 +.0)/nbResults)
    
  }
  
}

object tester {
  
  
	def main(args : Array[String]) {
	  println("tester valid solution" )
	  val generator = new BinPackingValidInstanceGenerator()
	  
 
	  val instance = new BinPackingInstance();
						
  instance.binCapacities = Array(2 to 3,45 to 55,0 to 0)
 instance.itemsSizes = Array(3,10,12,18,10)
 instance.binForItems = Array(
			Array(0,1),
			Array(1,2),
			Array(1,2),
			Array(1,0),
			Array(1,0))						
	  
	  val r = new Results()
	  
	  for(instance <- generator.generate.take(1000))
	  {
	    
		  instance.shuffle
	  

	    val resultShaw = (new InstanceSolver(instance)).solve(true,false,false)
	    val resultCurrentFlow = (new InstanceSolver(instance)).solve(true,true,false)
	    val resultExtFlow = (new InstanceSolver(instance)).solve(true,false,true)
	    val results = resultShaw :: resultCurrentFlow :: resultExtFlow :: Nil
	    val nbSol = results.head._1 
	    println( results .mkString(" "))
		r.addResult(results.toArray)
	    if(results.exists(_._1 != nbSol))
	    {
	      println(instance.description())
	      throw new Exception("all constraint don't give the same result")
	    }
	    
	    
	  }
  
	  println("means : " + r.total.mkString(""))
	  
	  
	}
}