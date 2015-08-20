package oscar.des.flow.core

import scala.collection.SortedSet
import scala.collection.immutable.BitSet

class AttributeDefinitions(l:List[String]){
  def size:Int = l.length
}

class Attribute(val id:Int,val convention:AttributeDefinitions) extends Ordered[Attribute]{
  def mask:Int = 1 << id
  override def compare(that: Attribute): Int = this.id.compare(that.id)
}

case class AttributeSet(attributes:SortedSet[Attribute], val convention:AttributeDefinitions){
  def mask:Int = attributes.foldLeft(0)((acc,l) => acc | l.mask)
}

object AttributeHelper{
  def unionAll(l:Set[AttributeSet]):AttributeSet =
    AttributeSet(l.foldLeft(SortedSet.empty[Attribute])((acc,l) => acc union l.attributes), l.head.convention)

  def fullSet(a:AttributeDefinitions):AttributeSet = null
  def emptySet(a:AttributeDefinitions):AttributeSet = null
  def attributeList(d:AttributeDefinitions):List[Attribute] = null
}

// //////////////////////////////////////////////////////////////
// A2A
sealed abstract class A2ATransformFunction{
  def apply(l:AttributeSet):AttributeSet

  //1: conjunction 2:disjunction
  def conjunctionMask:Int = -1
  def disjunctionMask:Int = 0

  def quickApply(i:Int):Int = (i & conjunctionMask) | disjunctionMask
}

class RemoveAttribute(a:Attribute) extends A2ATransformFunction{
  override def apply(l:AttributeSet):AttributeSet = AttributeSet(l.attributes.-(a),l.convention)
  override val conjunctionMask:Int = -1 ^ (1 << a.id)
}

class AddAttribute(a:Attribute) extends A2ATransformFunction{
  override def apply(l:AttributeSet):AttributeSet = AttributeSet(l.attributes.+(a),l.convention)
  override val disjunctionMask:Int = (1 << a.id)
}

class ConstantAttributes(s:AttributeSet) extends A2ATransformFunction{
  override def apply(l:AttributeSet):AttributeSet = s
  val mask:Int = s.mask
  override def conjunctionMask:Int = mask
  override def disjunctionMask:Int = mask
}

class ComposedA2A(ops:List[A2ATransformFunction]) extends A2ATransformFunction{
  def apply(l:AttributeSet):AttributeSet = ops.foldLeft(l)((acc,op) => op(acc))
  def quickApplyNoCache(i:Int):Int = ops.foldLeft(i)((acc,op) => op.quickApply(acc))
  override val conjunctionMask:Int = quickApplyNoCache(-1)
  override val disjunctionMask:Int = quickApplyNoCache(0)
}
