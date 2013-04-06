package oscar.examples.cp

import oscar.cp.core.CPVarInt
import oscar.cp.constraints.BinPacking
import oscar.cp.modeling.CPSolver
import oscar.cp.constraints.Sum

object BinPackingFail {
  def main(args : Array[String]) {
    
	  val binCapacities = Array(2 to 3,45 to 55,0 to 0)
	  val itemsSizes = Array(3,10,12,18,10)
	  val binForItems = Array(
				Array(0,1),
				Array(1,2),
				Array(1,2),
				Array(1,0),
				Array(1,0))
					
	  val items = 0 until itemsSizes.length
	  val bins = 0 until binCapacities.length
	  
	  val cp = new CPSolver()
		  val x = (for(i <- items) yield CPVarInt(cp, binForItems(i))).toArray
		  val l = bins.map(i=>CPVarInt(cp,binCapacities(i))).toArray
		  
		
		  	
		  cp.solve subjectTo {
			 cp.add(new BinPacking(x,itemsSizes,l))
/*
			 val itemInBin = Array.tabulate(items.size, bins.size)((i,j) => CPVarInt(cp,Array(0,1)))
			 for (j <- bins; i<- items)
			   cp.add(itemInBin(i)(j) == x(i).isEq(j))
			 for(j <- bins)
				 cp.add(new Sum((for(i<-items) yield itemInBin(i)(j) * itemsSizes(i)).toArray,l(j)))
*/				 
			} exploration { 
			  cp.binaryFirstFail(x)
			  println("solution x:"+x.map(_.getValue).mkString(" ") + " l" + l.map(_.getValue).mkString(" ") )
		    } run()
	}
}
