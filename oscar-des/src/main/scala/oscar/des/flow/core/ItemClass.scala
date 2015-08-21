package oscar.des.flow.core

import scala.collection.immutable.BitSet
import scala.collection.mutable.ArrayBuffer

//this is the first version, that relies on attributes.


object ItemClassHelper{
  type ItemClass = Int
  val zeroItemClass = 0

  def unionAll(l:List[ItemClass]):ItemClass = l.foldLeft(0:ItemClass)((acc,l) => l & acc)
  def union(i:ItemClass,j:ItemClass):ItemClass = i | j

  def mkFunction(a:ItemClassTransformFunction):(ItemClass => ItemClass) = a.quickApply

  implicit def storage(i:(Int,ItemClass)):List[ItemClass] = (1 to i._1).map(_=>i._2).toList
}