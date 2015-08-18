package oscar.des.flow.core

import scala.collection.immutable.BitSet
import scala.collection.mutable.ArrayBuffer

//this is the first version, that relies on attributes.
class ItemClass(as:AttributeSet){
  def union(b:ItemClass):ItemClass = null
}


object ItemClassHelper{
  def unionAll(l:List[ItemClass]):ItemClass = l.foldLeft(null:ItemClass)((acc,l) => l union acc)
}