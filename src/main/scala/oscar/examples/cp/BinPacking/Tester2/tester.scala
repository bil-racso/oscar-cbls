package oscar.examples.cp.BinPacking.Tester2

import oscar.examples.cp.BinPacking.Tester2.InstanceSolver
import oscar.examples.cp.BinPacking.DataStructures.BinPackingInstance
import java.io.FileWriter



class Tester2Results {
  var results = Array[(Int,Long,Int)]()
  var max = Array[(Int,Long,Int)]()
  var min = Array[(Int,Long,Int)]()
  var nbResults = 0
  
  
  def keys   = "nbResults":: results.zipWithIndex.map(i=>"resutls" + i._2).toList
  def values = nbResults  :: results.zipWithIndex.map(i=>i._1).toList
  
  
  def addResult(result : Array[(Int,Long,Int)])
  {
    
    if (nbResults == 0)
    {
      results = result.clone
      max = result.clone
      min = result.clone
    }
    else
    {
	    for ((r:(Int,Long,Int), i:Int) <- result.zipWithIndex)
	    {
	      results(i) = (results(i)._1 + r._1,results(i)._2 + r._2,results(i)._3 + r._3)
	      max(i) = (Math.max(max(i)._1, r._1),Math.max(max(i)._2, r._2),Math.max(max(i)._3, r._3))
	      min(i) = (Math.min(min(i)._1, r._1),Math.min(min(i)._2, r._2),Math.min(min(i)._3, r._3))
	    }
    }
      
    nbResults += 1
  }
  
  def total() = {
    for (r:(Int,Long,Int) <- results)
      yield ((r._1 +.0)/nbResults, (r._2 +.0)/nbResults, (r._3 +.0)/nbResults)
    
  }
  
}

object tester {

  def test(generator : BinPackingValidInstanceGenerator) =
  {
      val r = new Tester2Results()
	  
	  for(instance <- generator.generate.take(100))
	  {
		instance.shuffle
	    val resultShaw = (new InstanceSolver(instance)).solve(true,false,false)
	    val resultCurrentFlow = (new InstanceSolver(instance)).solve(true,true,false)
	    val resultExtFlow = (new InstanceSolver(instance)).solve(true,false,true)
	    val results = resultShaw :: resultCurrentFlow :: resultExtFlow :: Nil
	    val nbSol = results.head._1 
	    println( results .mkString(" "))
		r.addResult(results.toArray)
	    
	  }
      r
  }
    
  def logResult(generator:BinPackingValidInstanceGenerator, result:Tester2Results) =
  {
    this.synchronized 
    {
	    val resultFilePath = "tester2Results.xml"
	     val fw = new FileWriter(resultFilePath, true)
		  try {
			  val line:String = generator.values.mkString("\t") + ";;" + result.values.mkString("\t") + "\n"
			  fw.write(line)
		  }
		  finally fw.close() 
		  true
    }
  }
  
	def main(args : Array[String]) {
	  println("tester valid solution")
	  var generators =List[BinPackingValidInstanceGenerator]()
	  /*
	  for(i<- 1.to(30,5)) {
	    val generator = new BinPackingValidInstanceGenerator()
	    generator.binByItemMean = i
	  } 
	  
	  for(i<- 1.to(30,5)) {
	    val generator = new BinPackingValidInstanceGenerator()
	    generator.binByItemMean = i
	    generators ::=  generator
	  } 
	  
	  
	  for(i<- 0.to(10)) {
	    val generator = new BinPackingValidInstanceGenerator()
	    generator.itemsSizeDeviation = i
	    generators ::=  generator
	  } 
	  
	  for(i<- (0.00).to(0.2,0.02)) {
	    val generator = new BinPackingValidInstanceGenerator()
	    generator.binSizeDomainExpension = i
	    generators ::=  generator
	  } 
	  for(i<- 1.to(5)) {
	    val generator = new BinPackingValidInstanceGenerator()
	    generator.binByItemMean = i
	    generators ::=  generator
	  } 
	  */
	  	    val generator = new BinPackingValidInstanceGenerator()
	    generator.binByItemMean = 6
	    generators ::=  generator
	  generators.par.map(generator => logResult(generator,test(generator)))
	  
	  
	}
}