package oscar.lcg.support

import oscar.lcg.core.Literal
import oscar.algo.array.ArrayStack

class Builder {
  var locked = false;
  val reason: ArrayStack[Literal] = new ArrayStack[Literal](100)
  
  def lock(){
    if(locked)throw new Error()
    locked = true;
  }
  def release(){
    locked = false;
    reason.clear()
  }
  def add(v:Literal){
	//TODO: Test that this is not a TRUE lit
	reason.append(v)
  }
  def isEmpty(): Boolean = reason.isEmpty
  def size(): Int = reason.size
  def array(): Array[Literal] = {
    
    Array.tabulate(size())(reason(_))//TODO: This method copies the array, so we miss all the point of the builder!
    
  }
}

object Builder{
  val b = new Builder()
  def apply():Builder = b
}