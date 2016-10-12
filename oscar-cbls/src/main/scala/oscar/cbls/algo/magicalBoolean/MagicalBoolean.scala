package oscar.cbls.algo.magicalBoolean


/**
  * Created by Jannou Brohée on 3/10/16.
  */

/**
  *
  * @param length Maximum length of magical array
  */
class MagicalBoolean(val length:Int){

  //
  val threshold:Int = Int.MaxValue-1
  //
  var global:Int = 1
  //
  var internalArray :Array[Int] = Array.ofDim[Int](size)

  /**
    * Set the new value of element at specific index
    * @param id the index of the element
    * @param value the new element's value (true/false)
    * @note in O(1) // trivial
    */
  def set(id:Int, value:Boolean): Unit ={
    assert(id<size && 0<=id)
    if(value)internalArray(id)=global else internalArray(id)=global-1
  }

  /**
    * Return the value of the element at specific index
    * @param id the index of the element
    * @return true or false
    * @note in O(1) // trivial
    */
  def get(id:Int): Boolean ={
    assert(0<=id && id<size)
    !(internalArray(id)<global)
  }

  /**
    * Set to false the value of each element
    * @note O(1) (one var to update ==> O(1) or two vars to update (2*O(1) = O(1)) + one "if condition" test => O(1)
    */
  def setAll(): Unit ={
    if(Math.abs(global)==threshold){
      global = 0
      internalArray = Array.ofDim[Int](size)
    }else{
      global= -Math.abs(global)
    }
  }

  /**
    * Set to true the value of each element
    * @note O(1) (one var to update ==> O(1) or two vars to update (2*O(1) = O(1)) + one "if condition" test => O(1)
    */
  def clearAll(): Unit ={
    if(Math.abs(global)==threshold){
      global =1
      internalArray = Array.ofDim[Int](size)
    }else{
      global= Math.abs(global)+1
    }
  }

  /**
    * Creates a new iterator over the indexes of elements which value is true.
    * @return the new iterator
    */
  def iterator:Iterator[Int] ={
    val ret=Array.newBuilder[Int]
    for(n <-0 until length-1){
      if(internalArray(n)>=global){
        ret.+=(n)
      }
    }
    ret.result().iterator
  }

  override def toString: String = "["+iterator.mkString(",")+"]"

  /**
    * The length of the Magical Array Of Boolean.
    * @return the number of elements in this Magical Array Of Boolean.
    */
  def size :Int  = length
}


object MagicalBoolean {

  /**
    * create a Magical Array Of Boolean of given length
    * @param n the length
    * @return a Magical Array Of Boolean or null if length is less than zero
    */
  def apply(n:Int):MagicalBoolean ={
    if(n>=0) new MagicalBoolean(n) else null
  }



}
