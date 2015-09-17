package oscar.des.flow.modeling

import oscar.des.flow.core.ItemClassHelper.ItemClass
import oscar.des.flow.core._

import scala.collection.SortedSet
import scala.language.implicitConversions

/**
 * Created by rdl on 31/08/2015.
 */
trait AttributeHelper {
  /**
   * defines an attribute symbol table.
   * this serves to attribute an intr value to each attribute
   * @param l
   */
  def attributeDefinitions(l: String*) = new AttributeDefinitions(l: _*)

  /**
   * this represents a set of attributes, or an ItemClass
   * @param attributes
   * @param convention
   */
  def attributeSet(attributes: SortedSet[Attribute], convention: AttributeDefinitions)
  = new AttributeSet(attributes, convention)

  /**
   * a function that removes an atribute from an attribute set
   * @param a the attribute to remove
   */
  def removeAttribute(a: Attribute) = RemoveAttribute(a)

  /**
   * a function that adds an attribute from an attribute set
   * @param a the attribute to add
   */
  def addAttribute(a: Attribute) = AddAttribute(a: Attribute)

  /**
   * a function that returns a pre-defined attribute set; ignoring the attributes passed to it
   * @param s the attributes to return
   */
  def constantAttributes(s: AttributeSet) = ConstantAttributes(s: AttributeSet)

  /**
   * a composite function of attribute transforms.
   * the functions are called one after the other to gradually transform attributeSet into their output value
   * @param ops the sequence of transforms to apply, in this order
   */
  def composedItemClassTransformFunction(ops: ItemClassTransformFunction*) = ComposedItemClassTransformFunction(ops: _*)

  /**
   * ths identify tranform fucntion for attribute sets
   */
  val identity = new Identity()

  /**
   * this is an if-then-else construct
   * @param a the attribute on which the if-then else is targeted
   * @param thenBranch the then branch, taken if a is in the submitted attribute  set
   * @param elseBranch the else branch, taken if a is not in the submitted attribute set
   */
  def iTE(a: Attribute, thenBranch: ItemClassTransformWitAdditionalOutput, elseBranch: ItemClassTransformWitAdditionalOutput)
  = new ITE(a, thenBranch, elseBranch)

  /**
   * a function that defines the output integer sent back
   * @param outputValue
   * @param attr
   */
  def outputValue(outputValue: () => Int, attr: ItemClassTransformFunction = Identity())
  = OutputValue(outputValue, attr)


  val zeroItemClass = 0

  implicit def storage(i:(Int,ItemClass)):List[ItemClass] = (1 to i._1).map(_=>i._2).toList
}