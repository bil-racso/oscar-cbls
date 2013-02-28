package oscar.examples.cp.BinPacking

class BinPackingInstanceGenerator
{
  

	var itemSizeMin					= 5
	var itemSizeMax					= Int.MaxValue
	var binCapacityMin				= 10
	var binCapacityMax				= Int.MaxValue //(not for the waste bin)

	
	
	var binCapacityMean				= 100
	var binCapacityDeviation		= 10
	
	var binCapacityDomainSizeMean  		= 20
	var binCapacityDomainSizeDeviation	= 2
	
	var itemsSizeMean				= 10
	var itemsSizeDeviation			= 4
	
	var numberOfBins				= 10
	var wasteBin 					= true
	
	//if we need 100 items to fill the bins there will be 150 availables 
	var itemAvailableToNeededRatio:Double 	= 1.5
	
	//for instance an item that need 10 items will have 15 available
	var itemAvailableToNeededRatioByBin:Double = 1.5
	
	def profileValues 	: Array[AnyVal] = Array(binCapacityMean, binCapacityDeviation,binCapacityDomainSizeMean,binCapacityDomainSizeDeviation, itemsSizeMean, itemsSizeDeviation, numberOfBins, wasteBin, itemAvailableToNeededRatio,itemAvailableToNeededRatioByBin)
	def profileKeys 	: Array[String] = Array("binCapacityMean", "binCapacityDeviation","binCapacityDomainSizeMean","binCapacityDomainSizeDeviation", "itemsSizeMean", "itemsSizeDeviation", "numberOfBins", "wasteBin", "itemAvailableToNeededRatio","itemAvailableToNeededRatioByBin")	
	
	
	def setProperties(profile : (Int,Int,Int,Int,Int,Int,Int,Boolean,Double,Double) ) {
	  binCapacityMean 					= profile._1
	  binCapacityDeviation				= profile._2
	  binCapacityDomainSizeMean			= profile._3
	  binCapacityDomainSizeDeviation	= profile._4
	  itemsSizeMean						= profile._5
	  itemsSizeDeviation				= profile._6
	  numberOfBins						= profile._7
	  wasteBin							= profile._8
	  itemAvailableToNeededRatio		= profile._9
	  itemAvailableToNeededRatioByBin	= profile._10
	}
	
	def generate () : Stream[BinPackingInstance] = 
	{
	  val r = new util.Random
	  val bpi = new BinPackingInstance()
	  
	  
	  
	  
	  bpi.binCapacities = Stream.continually{	val c = (r.nextGaussian * binCapacityDeviation + binCapacityMean);  
	  											val s = (r.nextGaussian * binCapacityDomainSizeDeviation + binCapacityDomainSizeMean)/ 2 .abs;
	  											(c - s).abs.toInt to (c + s).toInt}.filterNot(r => r.start < binCapacityMin || r.end > binCapacityMax).take(numberOfBins).toArray
	  
	  val numberOfItems = ((bpi.binCapacities.foldLeft(0)((s, r) => s + (r.start + r.end)/2 ) / itemsSizeMean) * itemAvailableToNeededRatio).toInt
	  
	  
	  
	  bpi.itemsSizes = Stream.continually{val c = ((r.nextGaussian * itemsSizeDeviation) + itemsSizeMean);  c.toInt}.filterNot(c => c < itemSizeMin || c > itemSizeMax).take(numberOfItems).toArray
	  
	  
	  val binItemProbabilities = Array.tabulate[Double](numberOfBins)
	  {
	    bin =>
	    val capacity = bpi.binCapacities(bin)	    
	    val numberOfItemsNeeded = (((capacity.start + capacity.end)/2.0) / itemsSizeMean ) * itemAvailableToNeededRatioByBin
		numberOfItemsNeeded / numberOfItems
	  }
	  
	  
	  bpi.binForItems = Array.tabulate[Array[Int]](numberOfItems)
	  {
  		item =>
		(bpi.binCapacities.indices).filter(a => r.nextDouble <= binItemProbabilities(a)).toArray
	  }
		
	  if(wasteBin) 
	  {
	    bpi.binCapacities = bpi.binCapacities :+ (0 to bpi.itemsSizes.foldLeft(0)(_ + _))
	    
	    //here the waste bin is the last one so the one with at index = to the precedent number of bins
	    bpi.binForItems = bpi.binForItems.map(_:+numberOfBins) //:+ bpi.itemsSizes.indices.toArray
	  }
	  bpi #:: generate()
	  
	}

}
