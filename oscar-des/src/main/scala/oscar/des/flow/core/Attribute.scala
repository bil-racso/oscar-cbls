package oscar.des.flow.core

import scala.collection.SortedSet
import scala.collection.immutable.{SortedMap, BitSet}

class AttributeDefinitions(l:String*){
  def size:Int = l.length
  val attributeArray:Array[Attribute] = {
    val numbering = l.toArray
    for(i <- numbering.indices) yield
    new Attribute(numbering(i),i,this)
  }.toArray

  val attributeDico:SortedMap[String,Attribute] =
    SortedMap.empty[String,Attribute].++(for (a <- attributeArray) yield (a.name,a))

  override def toString: String = {
    "AttributeDefinitions(" + l.mkString(",") + ")"
  }

  def get(s:String):Attribute = attributeDico.get(s) match{
    case Some(a) => a
    case None => throw new Error("unknown attribute:"+ s)
  }
}

class Attribute(val name:String, val id:Int,val convention:AttributeDefinitions) extends Ordered[Attribute]{
  def mask:Int = 1 << id
  override def compare(that: Attribute): Int = this.id.compare(that.id)

  override def toString: String = "Attribute(" + name + " id:" + id + ")"
}

object AttributeSet{
  def apply(convention:AttributeDefinitions, attributesString:String*) =
    new AttributeSet(SortedSet.empty[Attribute].++(attributesString.map((s: String) => (convention.get(s)))), convention)
}

case class AttributeSet(attributes:SortedSet[Attribute], val convention:AttributeDefinitions){
  def mask:Int = attributes.foldLeft(0)((acc,l) => acc | l.mask)

  override def toString: String = {
    "AttributeSet(" + attributes.map(_.name).mkString(",") + " " + convention + ")"
  }
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
sealed abstract class ItemClassTransformFunction{
  def apply(l:AttributeSet):AttributeSet

  //1: conjunction 2:disjunction
  def conjunctionMask:Int = -1
  def disjunctionMask:Int = 0

  def quickApply(i:Int):Int = (i & conjunctionMask) | disjunctionMask
}

case class RemoveAttribute(a:Attribute) extends ItemClassTransformFunction{
  override def apply(l:AttributeSet):AttributeSet = AttributeSet(l.attributes.-(a),l.convention)
  override val conjunctionMask:Int = -1 ^ (1 << a.id)
}

case class AddAttribute(a:Attribute) extends ItemClassTransformFunction{
  override def apply(l:AttributeSet):AttributeSet = AttributeSet(l.attributes.+(a),l.convention)
  override val disjunctionMask:Int = (1 << a.id)
}

case class ConstantAttributes(s:AttributeSet) extends ItemClassTransformFunction{
  override def apply(l:AttributeSet):AttributeSet = s
  val mask:Int = s.mask
  override def conjunctionMask:Int = mask
  override def disjunctionMask:Int = mask
}

case class ComposedItemClassTransformFunction(ops:ItemClassTransformFunction*) extends ItemClassTransformFunction{
  def apply(l:AttributeSet):AttributeSet = ops.foldLeft(l)((acc,op) => op(acc))
  def quickApplyNoCache(i:Int):Int = ops.foldLeft(i)((acc,op) => op.quickApply(acc))
  override val conjunctionMask:Int = quickApplyNoCache(-1)
  override val disjunctionMask:Int = quickApplyNoCache(0)
}

case class Identity() extends ItemClassTransformFunction{
  def apply(l:AttributeSet):AttributeSet = l
}

object Test extends App{
  val a = new AttributeDefinitions("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t")

  val transform = ComposedItemClassTransformFunction(RemoveAttribute(a.get("i")),
    AddAttribute(a.get("i")),
    AddAttribute(a.get("d")),
    RemoveAttribute(a.get("i")),
    AddAttribute(a.get("o")),
    RemoveAttribute(a.get("t")))

  val s = AttributeSet(a,"c","o","n")

  assert(transform.apply(s).mask == transform.quickApplyNoCache(s.mask))
  assert(transform.quickApplyNoCache(s.mask) == transform.quickApply(s.mask))
}

