package oscar.des.flow.core

import scala.collection.immutable.BitSet

class AttributeDefinitions(l:List[String]){
  def size:Int = l.length
}

class Attribute(val id:Int,val convention:AttributeDefinitions)

case class AttributeSet(val attributes:BitSet, val convention:AttributeDefinitions){
  def quickMask:Int =
}

object AttributeHelper{
  def unionAll(l:List[AttributeSet]):AttributeSet = AttributeSet(l.foldLeft(BitSet.empty)((acc,l) => acc union l.attributes), l.head.convention)

  def fullSet(a:AttributeDefinitions):AttributeSet = null
  def emptySet(a:AttributeDefinitions):AttributeSet = null
  def attributeList(d:AttributeDefinitions):List[Attribute] = null
}

// //////////////////////////////////////////////////////////////
// A2A
sealed abstract class A2ATransformFunction{
  def apply(l:AttributeSet):AttributeSet
  def quickApply(i:Int):Int
}

class RemoveAttribute(a:Attribute) extends A2ATransformFunction{
  override def apply(l:AttributeSet):AttributeSet = AttributeSet(l.attributes.-(a.id),l.convention)

  val conjunctionMask:Int = -1 ^ (1 << a.id)
  def quickApply(i:Int):Int = i & conjunctionMask
}

class AddAttribute(a:Attribute) extends A2ATransformFunction{
  override def apply(l:AttributeSet):AttributeSet = AttributeSet(l.attributes.+(a.id),l.convention)

  val conjunctionMask:Int = (1 << a.id)
  def quickApply(i:Int):Int = i | conjunctionMask
}

class ConstantAttributes(a:Attribute) extends A2ATransformFunction{
  override def apply(l:AttributeSet):AttributeSet = l

  val conjunctionMask:Int = (1 << a.id)
  def quickApply(i:Int):Int = i | conjunctionMask
}

class ComposedA2A(ops:List[A2ATransformFunction]) extends A2ATransformFunction{
  def apply(l:AttributeSet):AttributeSet = ops.foldLeft(l)((acc,op) => op(acc))
}
