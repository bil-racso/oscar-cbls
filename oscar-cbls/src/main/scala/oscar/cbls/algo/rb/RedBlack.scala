package oscar.cbls.algo.rb
/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/

import oscar.cbls.algo.quick.QList

/*** Okasaki-style red-black tree maps. ***/

private object RedBlackTreeMapLib{
  val R = true
  val B = false

  // blacken: Turn a node black.
  def blacken[V] (n : RedBlackTreeMap[V])  : RedBlackTreeMap[V] = {
    n match {
      case L() => n
      case T(_,l,k,v,r) => T(B,l,k,v,r)
    }
  }

  // balance: Balance a tree with balanced subtrees.
  def balance[V] (c : Boolean) (l : RedBlackTreeMap[V]) (k : Int) (v : Option[V]) (r : RedBlackTreeMap[V]) : RedBlackTreeMap[V] = {
    (c,l,k,v,r) match {
      case (B,T(R,T(R,a,xK,xV,b),yK,yV,c),zK,zV,d) => T(R,T(B,a,xK,xV,b),yK,yV,T(B,c,zK,zV,d))
      case (B,T(R,a,xK,xV,T(R,b,yK,yV,c)),zK,zV,d) => T(R,T(B,a,xK,xV,b),yK,yV,T(B,c,zK,zV,d))
      case (B,a,xK,xV,T(R,T(R,b,yK,yV,c),zK,zV,d)) => T(R,T(B,a,xK,xV,b),yK,yV,T(B,c,zK,zV,d))
      case (B,a,xK,xV,T(R,b,yK,yV,T(R,c,zK,zV,d))) => T(R,T(B,a,xK,xV,b),yK,yV,T(B,c,zK,zV,d))
      case (c,a,xK,xV,b) => T(c,a,xK,xV,b)
    }
  }
}

import RedBlackTreeMapLib._

//must use trait here because of specialization, so we ensure that this trait is compiled into a java interface by avoiding method code altogether. in the trait.
//as a consequence, there are duplicates in the classes implementing this trait.
trait RedBlackTreeMap[@specialized(Int) V]{

  /* We could have required that K be <: Ordered[K], but this is
  actually less general than requiring an implicit parameter that can
  convert K into an Ordered[K].

  For example, Int is not compatible with Ordered[Int].  This would
  make it unusable with this map; however, it's simple to define an
  injector from Int into Ordered[Int].

  In fact, the standard prelude already defines just such an implicit:
  intWrapper. */

  // modWith: Helper method; top node could be red.
  protected[rb] def modWith (k : Int, f : (Int, Option[V]) => Option[V]) : RedBlackTreeMap[V]

  // get: Retrieve a value for a key.
  def get(k : Int) : Option[V]

  def getOrElse(k:Int,default: =>V):V = get(k) match{
    case None => default
    case Some(x) => x
  }

  def contains(k:Int):Boolean

  def biggestLowerOrEqual(k:Int):Option[(Int,V)]

  protected[rb] def getBiggestLowerAcc(k:Int, bestoFar:(Int,V)):Option[(Int,V)]

  def smallestBiggerOrEqual(k:Int):Option[(Int,V)]

  def smallest:Option[(Int,V)]

  def biggest:Option[(Int,V)]

  def biggestPosition:Option[RBTMPosition[V]]

  def smallestPosition:Option[RBTMPosition[V]]

  protected[rb] def getSmallestBiggerAcc(k:Int, bestSoFar:(Int,V)):Option[(Int,V)]

  // insert: Insert a value at a key.
  def insert (k : Int, v : V) : RedBlackTreeMap[V]

  // remove: Delete a key.
  def remove (k : Int) : RedBlackTreeMap[V]

  def size:Int
  def isEmpty:Boolean

  def values:List[V]
  protected [rb] def valuesAcc(valuesAfter:List[V]):List[V]
  def content:List[(Int,V)]
  protected [rb] def contentAcc(valuesAfter:List[(Int,V)]):List[(Int,V)]

  def keys:List[Int]
  protected [rb] def keysAcc(keysAfter:List[Int]):List[Int]

  def positionOf(k: Int):Option[RBTMPosition[V]]
  protected[rb] def positionOfAcc(k:Int,positionAcc:QList[(T[V],Boolean)]):Option[RBTMPosition[V]]
}

// A leaf node.
case class L[@specialized(Int) V]() extends RedBlackTreeMap[V]  {


  def get(k : Int) : Option[V] = None

  override def contains(k : Int) : Boolean = false

  override protected[rb] def modWith (k : Int, f : (Int, Option[V]) => Option[V]) : RedBlackTreeMap[V] = {
    T(R, this, k, f(k,None), this)
  }

  def biggestLowerOrEqual(k:Int):Option[(Int,V)] = None

  override protected[rb] def getBiggestLowerAcc(k:Int, bestSoFar:(Int,V)) = Some(bestSoFar)

  override def smallestBiggerOrEqual(k: Int):Option[(Int,V)] = None

  override protected[rb] def getSmallestBiggerAcc(k: Int, bestSoFar: (Int, V)) = Some(bestSoFar)

  override def size: Int = 0
  override def isEmpty = true

  protected [rb] def valuesAcc(valuesAfter:List[V]):List[V] = valuesAfter
  protected [rb] def contentAcc(valuesAfter:List[(Int,V)]):List[(Int,V)] = valuesAfter
  protected [rb] def keysAcc(keysAfter:List[Int]):List[Int] = keysAfter

  protected[rb] override def positionOfAcc(k : Int, positionAcc : QList[(T[V],Boolean)]) : Option[RBTMPosition[V]] = None


  //duplicates
  def values:List[V] = List.empty

  def content:List[(Int,V)] = List.empty

  override def keys : List[Int] = List.empty

  override def positionOf(k: Int):Option[RBTMPosition[V]] = None

  // insert: Insert a value at a key.
  override def insert (k : Int, v : V) =  T(B, L(), k , Some(v), L())

  // remove: Delete a key.
  override def remove (k : Int) = this

  override def smallest:Option[(Int,V)] = None

  override def biggest:Option[(Int,V)] = None

  override def biggestPosition:Option[RBTMPosition[V]] = None

  override def smallestPosition:Option[RBTMPosition[V]] = None
}

// A tree node.
case class T[@specialized(Int) V](c : Boolean, l : RedBlackTreeMap[V], k : Int, v : Option[V], r : RedBlackTreeMap[V]) extends RedBlackTreeMap[V] {
  lazy val mSize = l.size + r.size + 1
  override def size = mSize
  override def isEmpty = false

  def get(k : Int) : Option[V] = {
    if (k < this.k) l.get(k)
    else if (k > this.k) r.get(k)
    else v
  }

  override def contains(k : Int) : Boolean = {
    if (k < this.k) l.contains(k)
    else if (k > this.k) r.contains(k)
    else true
  }

  def biggestLowerOrEqual(k:Int):Option[(Int,V)] = {
    if (k < this.k) l.biggestLowerOrEqual(k)
    else if (this.k < k) r.getBiggestLowerAcc(k,(this.k,v.head))
    else Some(k,v.head)
  }

  override protected[rb] def getBiggestLowerAcc(k:Int, bestSoFar:(Int,V)):Option[(Int,V)] = {
    if (k < this.k) l.getBiggestLowerAcc(k, bestSoFar)
    else if (this.k < k) r.getBiggestLowerAcc(k, (this.k, v.head))
    else Some(k,v.head)
  }

  override def smallestBiggerOrEqual(k: Int):Option[(Int,V)] = {
    if (k < this.k) l.getSmallestBiggerAcc(k, (this.k, v.head))
    else if (this.k < k) r.smallestBiggerOrEqual(k)
    else Some(k,v.head)
  }

  override protected[rb] def getSmallestBiggerAcc(k: Int, bestSoFar: (Int,V)):Option[(Int,V)] = {
    if (k < this.k) l.getSmallestBiggerAcc(k, (this.k, v.head))
    else if (this.k < k) r.getSmallestBiggerAcc(k, bestSoFar)
    else Some(k,v.head)
  }

  override protected[rb] def modWith (k : Int, f : (Int, Option[V]) => Option[V]) : RedBlackTreeMap[V] = {
    if (k <  this.k) balance (c) (l.modWith(k,f)) (this.k) (this.v) (r)
    else if (k == this.k) {
      f(this.k, this.v) match{
        case None =>
          if(l.isEmpty) r
          else if (r.isEmpty) l
          else{
            r.smallest match{
              case Some((k,v)) => T(c, l, k, Some(v), r.remove(k))
              case None => throw new Error("non smallest on non-empty RB?")
            }
          }
        case x => T(c, l, k, x, r)
      }
    }else {
      balance(c)(l)(this.k)(this.v)(r.modWith(k, f))
    }
  }

  override protected[rb] def valuesAcc(valuesAfter : List[V]) : List[V] = l.valuesAcc(v.head :: r.valuesAcc(valuesAfter))

  protected [rb] def contentAcc(valuesAfter:List[(Int,V)]):List[(Int,V)] = l.contentAcc((k,v.head) :: r.contentAcc(valuesAfter))

  protected [rb] def keysAcc(keysAfter:List[Int]):List[Int] = l.keysAcc(k :: r.keysAcc(keysAfter))

  protected[rb] override def positionOfAcc(k : Int, positionAcc : QList[(T[V],Boolean)]) : Option[RBTMPosition[V]] = {
    if (k < this.k) l.positionOfAcc(k, QList((this,false),positionAcc))
    else if (k > this.k) r.positionOfAcc(k, QList((this,true),positionAcc))
    else Some(new RBTMPosition[V](QList((this,true),positionAcc)))
  }

  def hasLeft:Boolean = l.isInstanceOf[T[V]]
  def hasRight:Boolean = r.isInstanceOf[T[V]]

  //duplicates
  override def values:List[V] = valuesAcc(List.empty)

  override def content : List[(Int, V)] = contentAcc(List.empty)

  override def keys : List[Int] = keysAcc(List.empty)

  override def positionOf(k: Int):Option[RBTMPosition[V]] = positionOfAcc(k:Int,null)

  // insert: Insert a value at a key.
  override def insert (k : Int, v : V) = blacken(modWith(k, (_,_) => Some(v)))

  // remove: Delete a key.
  override def remove (k : Int) = blacken(modWith(k, (_,_) => None))

  override def smallest:Option[(Int,V)] = smallestBiggerOrEqual(Int.MinValue)

  override def biggest:Option[(Int,V)] = biggestLowerOrEqual(Int.MaxValue)

  override def biggestPosition:Option[RBTMPosition[V]] = {
    biggestLowerOrEqual(Int.MaxValue) match{
      case Some((k,_)) => positionOf(k)
      case None => None
    }
  }

  override def smallestPosition:Option[RBTMPosition[V]] = {
    smallestBiggerOrEqual(Int.MinValue) match{
      case Some((k,_)) => positionOf(k)
      case None => None
    }
  }

}

// A helper object.
object RedBlackTreeMap {

  // empty: Converts an orderable type into an empty RBMap.
  def empty[@specialized(Int) V] : RedBlackTreeMap[V] = L[V]()

  // apply: Assumes an implicit conversion.
  def apply[@specialized(Int) V](args : Iterable[(Int,V)]) : RedBlackTreeMap[V] = {
    var currentMap : RedBlackTreeMap[V] = L()
    for ((k,v) <- args) {
      currentMap = currentMap.insert(k,v)
    }
    currentMap
  }


  /**
    * make the red black tree out of already sorted couples (key,value)
    * they must be sorted by increasing order of key, and a key can only be present once.
    * There is no check of these properties
    * This is O(n); thus faster than a n*log(n) if you were building it from unsorted pairs
    * @param args
    * @tparam V
    * @return
    */
  def makeFromSorted[@specialized(Int) V](args:Iterable [(Int,V)]): RedBlackTreeMap[V] = {
    //root is to be black, beside alternate red and black
    val a = args.toArray
    if(args.size <=3) this.apply(args)
    else myMakeFromSorted(a,0,a.length-1,false)
  }

  /**
    * make the red black tree out of already sorted couples (key,value)
    * they must be sorted by increasing order of key, and a key can only be present once.
    * There is no check of these properties
    * This is O(n); thus faster than a n*log(n) if you were building it from unsorted pairs
    * @param args
    * @tparam V
    * @return
    */
  def makeFromSortedArray[@specialized(Int) V](args:Array[(Int,V)]): RedBlackTreeMap[V] = {
    //root is to be black, beside alternate red and black
    if(args.length <=3) this.apply(args)
    else myMakeFromSorted(args,0,args.length-1,false)
  }


  private def myMakeFromSorted[@specialized(Int) V](args:Array[(Int,V)],fromIncluded:Int,toIncluded:Int,targetIsRed:Boolean): RedBlackTreeMap[V] = {
    //root is to be black, beside alternate red and black
    if(fromIncluded == toIncluded){
      val(key,value) = args(fromIncluded)
      T(targetIsRed, L(),  key, Some(value), L())
    }else if (fromIncluded + 1 == toIncluded) {
      val(keyL,valueL) = args(fromIncluded)
      val(keyH,valueH) = args(toIncluded)
      T(targetIsRed, T(!targetIsRed, L(),  keyL, Some(valueL), L()),  keyH, Some(valueH), L())
    }else{
      //there is a middle point
      val middlePoint = (fromIncluded + toIncluded)/2
      val left = myMakeFromSorted(args,fromIncluded,middlePoint-1,!targetIsRed)
      val right = myMakeFromSorted(args,middlePoint+1,toIncluded,!targetIsRed)
      val(key,value) = args(middlePoint)
      T(targetIsRed, left,  key, Some(value), right)
    }
  }
}

//le booléen: true le noeud a déjà été montré (dans un parcour gauche à droite)
class RBTMPosition[@specialized(Int) V](position:QList[(T[V],Boolean)]){
  def key:Int = position.head._1.k
  def value:V = position.head._1.v.head

  override def toString : String = "RBPosition(key:" + key + " value:" + value + " stack:" + position.toList + ")"

  def next:Option[RBTMPosition[V]] = {

    def unstack1(position:QList[(T[V],Boolean)]):QList[(T[V],Boolean)] = {
      if (position == null) return null
      val head = position.head
      if (!head._2){
        //not presented yet, so we present this one
        QList((head._1,true),position.tail)
      }else{
        //already presented, so unstack
        unstack1(position.tail)
      }
    }

    def descendToLeftMost(position:QList[(T[V],Boolean)]):QList[(T[V],Boolean)] = {
      val headTree = position.head._1
      headTree.l match{
        case t:T[V] => descendToLeftMost(QList((t,false),position))
        case _ => QList((headTree,true),position.tail)
      }
    }

    val newStack = position.head._1.r match {
      case t : T[V] => descendToLeftMost(QList((t,false),position))
      case _ => unstack1(position)
    }

    if(newStack == null) None
    else Some(new RBTMPosition[V](newStack))
  }

  def prev:Option[RBTMPosition[V]] = {
    def unstack1(position:QList[(T[V],Boolean)]):QList[(T[V],Boolean)] = {
      if (position == null) return null
      val head = position.head
      if (head._2){
        //already presented, so roll back to it.
        QList((head._1,true),position.tail)
      }else{
        //already presented, so unstack
        unstack1(position.tail)
      }
    }

    def descendToRightMost(position:QList[(T[V],Boolean)]):QList[(T[V],Boolean)] = {
      val headTree = position.head._1
      headTree.r match{
        case t:T[V] => descendToRightMost(QList((t,true),position))
        case _ => QList((headTree,true),position.tail)
      }
    }

    val newStack = position.head._1.l match {
      case t : T[V] => descendToRightMost(QList((t,true),QList((position.head._1,false),position.tail)))
      case _ => unstack1(position.tail)
    }

    if(newStack == null) None
    else {
      assert(new RBTMPosition[V](newStack).next.head.key == this.key, "prev.next.key != this.key; this:" + this + " prev:" + new RBTMPosition[V](newStack))
      Some(new RBTMPosition[V](newStack))
    }
  }
}
