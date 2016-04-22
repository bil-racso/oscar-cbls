package oscar.des.flow.core

import oscar.des.flow.lib.{BoolExpr, DoubleExpr}

import scala.collection.immutable.SortedMap

class Properties {
  var doubleProperties:SortedMap[String,DoubleExpr] = SortedMap.empty
  var boolProperties:SortedMap[String,BoolExpr]= SortedMap.empty

  def addBoolProperty(name:String,expr:BoolExpr){
    boolProperties = boolProperties  + ((name,expr))
  }
  def addDoubleProperty(name:String,expr:DoubleExpr){
    doubleProperties = doubleProperties + ((name,expr))
  }

  def getDoubleProperty(name:String):DoubleExpr = {
    doubleProperties.get(name) match{
      case None => throw new Exception("cannot find arithmetic property " + name)
      case Some(e) => e
    }
  }

  def getBoolProperty(name:String):BoolExpr= {
    boolProperties.get(name) match{
      case None => throw new Exception("cannot find boolean property " + name)
      case Some(e) => e
    }
  }
}
