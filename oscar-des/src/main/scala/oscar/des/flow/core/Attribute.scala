package oscar.des.flow.core

import oscar.des.flow.core.ItemClassHelper.ItemClass

import scala.collection.SortedSet
import scala.collection.immutable.{SortedMap, BitSet}
import scala.language.implicitConversions


/**
 * defines an attribute symbol table.
 * this serves to attribute an intr value to each attribute
 * @param l
 */
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

  def getN(i:Int) = attributeArray(i)
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

/**
 * this represents a set of attributes, or an ItemClass
 * @param attributes
 * @param convention
 */
case class AttributeSet(attributes:SortedSet[Attribute], val convention:AttributeDefinitions){
  def mask:Int = attributes.foldLeft(0)((acc,l) => acc | l.mask)
  implicit def itemClass:ItemClass = mask
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

object ItemClassHelper{
  type ItemClass = Int
  val zeroItemClass = 0

  def unionAll(l:List[ItemClass]):ItemClass = l.foldLeft(0:ItemClass)((acc,l) => l & acc)
  def union(i:ItemClass,j:ItemClass):ItemClass = i | j
  implicit def storage(i:(Int,ItemClass)):List[ItemClass] = (1 to i._1).map(_=>i._2).toList
}

// //////////////////////////////////////////////////////////////
// A2A
sealed abstract class ItemClassTransformFunction{
  def slowApply(l:AttributeSet):AttributeSet

  //1: conjunction 2:disjunction
  def conjunctionMask:Int = -1
  def disjunctionMask:Int = 0

  def apply(i:Int):Int = (i & conjunctionMask) | disjunctionMask
}


/**
 * a function that removes an atribute from an attribute set
 * @param a the attribute to remove
 */
case class RemoveAttribute(a:Attribute) extends ItemClassTransformFunction{
  override def slowApply(l:AttributeSet):AttributeSet = AttributeSet(l.attributes.-(a),l.convention)
  override val conjunctionMask:Int = -1 ^ (1 << a.id)
}

/**
 * a function that adds an atribute from an attribute set
 * @param a the attribute to add
 */
case class AddAttribute(a:Attribute) extends ItemClassTransformFunction{
  override def slowApply(l:AttributeSet):AttributeSet = AttributeSet(l.attributes.+(a),l.convention)
  override val disjunctionMask:Int = (1 << a.id)
}

/**
 * a function that returns a pre-defined attribute set; ignoring the attributes passed to it
 * @param s the attributes to return
 */
case class ConstantAttributes(s:AttributeSet) extends ItemClassTransformFunction{
  override def slowApply(l:AttributeSet):AttributeSet = s
  val mask:Int = s.mask
  override def conjunctionMask:Int = mask
  override def disjunctionMask:Int = mask
}

/**
 * a composite function of attribute transforms.
 * the functions are called one after the other to gradually transform attributeSet into their output value
 * @param ops the sequence of transforms to apply, in this order
 */
case class ComposedItemClassTransformFunction(ops:ItemClassTransformFunction*) extends ItemClassTransformFunction{
  def slowApply(l:AttributeSet):AttributeSet = ops.foldLeft(l)((acc,op) => op.slowApply(acc))
  def quickApplyNoCache(i:Int):Int = ops.foldLeft(i)((acc,op) => op.apply(acc))
  override val conjunctionMask:Int = quickApplyNoCache(-1)
  override val disjunctionMask:Int = quickApplyNoCache(0)
}

/**
 * ths identify tranform fucntion for attribute sets
 */
case class Identity() extends ItemClassTransformFunction{
  def slowApply(l:AttributeSet):AttributeSet = l
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

  assert(transform.slowApply(s).mask == transform.quickApplyNoCache(s.mask))
  assert(transform.quickApplyNoCache(s.mask) == transform.apply(s.mask))
}

sealed abstract class ItemClassTransformWitAdditionalOutput{
  def slowApply(l:AttributeSet):(Int,AttributeSet)
  def apply(i:Int):(Int,Int)
}

/**
 * this is an if-then-else construct
 * @param a the attribute on which the if-then else is targeted
 * @param thenBranch the then branch, taken if a is in the submitted attribute  set
 * @param elseBranch the else branch, taken if a is not in the submitted attribute set
 */
case class ITE(a:Attribute,thenBranch:ItemClassTransformWitAdditionalOutput,elseBranch:ItemClassTransformWitAdditionalOutput) extends ItemClassTransformWitAdditionalOutput{
  val theMask:Int = a.mask
  override def slowApply(l: AttributeSet): (Int, AttributeSet) = {
    if (l.attributes.contains(a)){
      thenBranch.slowApply(l)
    }else{
      elseBranch.slowApply(l)
    }
  }

  override def apply(i: Int): (Int, Int) = {
    if((i & theMask) != 0){
      thenBranch.apply(i)
    }else{
      elseBranch.apply(i)
    }
  }
}

/**
 * a function that defines the output integer sent back
 * @param outputValue
 * @param attr
 */
case class OutputValue(outputValue:()=>Int,attr:ItemClassTransformFunction = Identity()) extends ItemClassTransformWitAdditionalOutput{
  override def slowApply(l: AttributeSet): (Int, AttributeSet) =
    (outputValue(),attr.slowApply(l))

  override def apply(i: Int): (Int, Int) = {
    (outputValue(),attr(i))
  }
}