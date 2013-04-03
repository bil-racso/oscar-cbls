package oscar.examples.cp.BinPacking.DataStructures

import scala.Array.canBuildFrom

class BinPackingInstance
{
    var binCapacities = Array[Range]() 
    var itemsSizes = Array[Int]()
    var binForItems = Array[Array[Int]]() 
    
    def items = 0 until itemsSizes.length
    def bins = 0 until binCapacities.length
    
    def description(instName:String = "instance") = 
    {
      var str = "BinPackingInstance with " + binCapacities.length + " bins and " + itemsSizes.length + " items"
      str += "\n "+instName+".binCapacities = Array(" + binCapacities.map(r => r.start + " to " + r.end).mkString(",")+")"
      str += "\n "+instName+".itemsSizes = Array(" + itemsSizes.mkString(",")+")"
      str += "\n "+instName+".binForItems = Array(" + binForItems.map("\n\t\t\tArray(" + _.mkString(",") + ")").mkString(",")+")"
      str += "\n"
      str
      
    }
    
}

