package oscar.flatzinc.model

import scala.collection.mutable.{ Map => MMap, Set => MSet }

class FZSolution {
    var output:MMap[String, (String => String) => String]= MMap.empty
    
    def addOutputVarInt(label: String, id:String) = {
      def k(c:(String => String)) = c(id)
      output += label -> k
    }
    def addOutputVarBool(label: String, id:String) = {
      def k(c:(String => String)) = if (c(id).equals("0")) "false" else "true"
      output += label -> k
    }
    def addOutputArrayVarInt(label:String,vars:Array[String],dimRanges: List[Range]) = {
      def k(c:(String => String)) = {
        "array"+dimRanges.length+"d("+dimRanges.map(r => r.min+".."+r.max).mkString(", ")+", ["+vars.map(c(_)).mkString(", ")+" ]);"
      }
      output += label -> k
    }
    def addOutputArrayVarBool(label:String,vars:Array[String],dimRanges: List[Range]) = {
      def k(c:(String => String)) = {
        "array"+dimRanges.length+"d("+dimRanges.map(r => r.min+".."+r.max).mkString(", ")+", ["+vars.map(v => if(c(v).equals("0")) "false" else "true").mkString(", ")+" ]);"
      }
      output += label -> k
    }
    
    def getSolution(converter:String=>String):String = {
      output.map{case (id,f) => id+" = "+f(converter)+"\n"}.mkString("")+"\n----------\n"
    }
    
	def handleSolution(converter:(String)=>String) = {
	  println(getSolution(converter));
	}
}