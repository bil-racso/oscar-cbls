package oscar.examples.cp.BinPacking

class BinPackingInstance
{
    var binCapacities = Array[Range]() 
    var itemsSizes = Array[Int]()
    var binForItems = Array[Array[Int]]() 
    
    def items = 0 until itemsSizes.length
    def bins = 0 until binCapacities.length
    
    def description() = 
    {
      var str = "BinPackingInstance with " + binCapacities.length + " bins and " + itemsSizes.length + " items"
      str += "\n bin capacities : " + binCapacities.map(r => r.start + " to " + r.end).mkString(",")
      str += "\n items sizes : " + itemsSizes.mkString(",")
      str += "\n bins for items : " + binForItems.map("\n\tArray(" + _.mkString(",") + ")").mkString(",")
      str += "\n"
      str
      
    }
}

