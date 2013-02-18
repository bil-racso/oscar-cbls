/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

package oscar.cbls.invariants.core.algo.dll

/**this is a mutable data strcuture that is able to represent sets through doubly-lined lists, with insert and delete in O(1) through reference
 * and to update in parallell another set that is a filter of the first one through a specified function
 * the filter can be specified anytime and filtering can be cascated, but a PermaFilteresDLL can have only one filter
 *
 * You should not perform any operation on the slave DLL,
 * although this will not be detected and reported as an error
 *
 * Beware that this is a mutable data structure, hence you should not perform any update on it while iterating on it.
 * */
 class PermaFilteredDoublyLinkedList[T <: AnyRef] extends Iterable[T]{

  private val headfantom:PFDLLStorageElement[T] = new PFDLLStorageElement[T](null.asInstanceOf[T])
  private val endfantom:PFDLLStorageElement[T] = new PFDLLStorageElement[T](null.asInstanceOf[T])
  headfantom.setNext(endfantom)

  private var mfilter:T => Boolean = null
  private var mmap:T => AnyRef = null
  private var Filtered: PermaFilteredDoublyLinkedList[AnyRef] = null

   /**returns the size of the PermaFilteredDLL*/
  override def size = msize

  private var msize:Int = 0

   /**adds an a item in the PermaFilteredDLL, and if accepted by the filter, adds it in the slave PermaFilteredDLL.
    * returns a reference that should be used to remove the item from all those structures at once.
    */
  def addElem(elem:T):PFDLLStorageElement[T] = {
    val d = new PFDLLStorageElement[T](elem)
    d.setNext(headfantom.next)
    headfantom.setNext(d)
    msize +=1
    if(mfilter != null && mfilter(elem)){
      if(mmap == null){
        d.filtered = Filtered.addAny(elem)
      }else{
        d.filtered = Filtered.addAny(mmap(elem))
      }
    }
    d
  }

  def addAny(elem:AnyRef):AnyRef =  addElem(elem.asInstanceOf[T])

  /**adds an element to the data structure, cfr. method addElem*/
  def +(elem:T){addElem(elem)}

  /**adds a bunch of items to the data structures*/
  def ++(elems:Iterable[T]) {for(elem <- elems) addElem(elem)}

   /**deletes an item from the DLL and all the filtered DLL.
    * the item is specified through the reference given when it was inserted in the first place.
    */
  def deleteElem(elemkey:PFDLLStorageElement[T]):T = {
    elemkey.prev.setNext(elemkey.next)
    msize -=1
    if(mfilter != null && elemkey.filtered != null){
      Filtered.deleteAny(elemkey.filtered)
    }
    elemkey.elem
  }

   def deleteAny(elemkey:AnyRef):AnyRef = deleteElem(elemkey.asInstanceOf[PFDLLStorageElement[T]])

   /**makes the DLL empty, and all its filtered DLL as well*/
  def dropAll(){
    headfantom.setNext(endfantom)
    msize = 0
    if(mfilter != null){
      Filtered.dropAll()
    }
  }

  override def isEmpty:Boolean = (size == 0)

  override def iterator = new PFDLLIterator[T](headfantom,endfantom)

  def PermaFilter[X <: AnyRef](filter:T => Boolean, map:T => X = null):PermaFilteredDoublyLinkedList[X] = {
    assert(mfilter == null,"PermaFilteredDoublyLinkedList can only accept a single filter")
    mfilter = filter
    val newlist = new PermaFilteredDoublyLinkedList[X]
    Filtered = newlist.asInstanceOf[PermaFilteredDoublyLinkedList[AnyRef]]
    mmap = map

    var currentstorageElement:PFDLLStorageElement[T]=headfantom.next
    while(currentstorageElement!=endfantom){
      if(mfilter(currentstorageElement.elem)){
        if(map == null){
          currentstorageElement.filtered = Filtered.addAny(currentstorageElement.elem)
        }else{
          currentstorageElement.filtered = Filtered.addAny(map(currentstorageElement.elem))
        }
      }
      currentstorageElement = currentstorageElement.next
    }
    newlist
  }

   /**
    * @param fn the function to execute on each items included in this list
    * @tparam U the output type of the function
    * @return a list containing the result of executing fn on each element of the DLL. the list is in reverse order.
    */
   def mapToList[U](fn:T => U):List[U] = {
     val it = iterator
     var toReturn:List[U] = List.empty
     while(it.hasNext){
       toReturn = fn(it.next()) :: toReturn
     }
     toReturn
   }
}

class PFDLLStorageElement[T](val elem:T){
  var next:PFDLLStorageElement[T] = null
  var prev:PFDLLStorageElement[T] = null
  var filtered: AnyRef = null

  def setNext(d:PFDLLStorageElement[T]){
    this.next = d
    d.prev = this
  }
}

class PFDLLIterator[T](var CurrentKey:PFDLLStorageElement[T], val endfantom:PFDLLStorageElement[T]) extends Iterator[T]{
  def next():T = {
    CurrentKey = CurrentKey.next
    CurrentKey.elem
  }

  def hasNext:Boolean = {CurrentKey.next != endfantom}
}
