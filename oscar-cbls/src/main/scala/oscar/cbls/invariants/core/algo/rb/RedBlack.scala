package oscar.cbls.invariants.core.algo.rb

import oscar.cbls.invariants.core.algo.quick.QList

/*** Okasaki-style red-black tree maps. ***/

abstract class RedBlackTree[V]{

  protected val R = true
  protected val B = false

  /* We could have required that K be <: Ordered[K], but this is
  actually less general than requiring an implicit parameter that can
  convert K into an Ordered[K].

  For example, Int is not compatible with Ordered[Int].  This would
  make it unusable with this map; however, it's simple to define an
  injector from Int into Ordered[Int].

  In fact, the standard prelude already defines just such an implicit:
  intWrapper. */

  // blacken: Turn a node black.
  protected def blacken (n : RedBlackTree[V])  : RedBlackTree[V] = {
    n match {
      case L() => n
      case T(_,l,k,v,r) => T(B,l,k,v,r)
    }
  }

  // balance: Balance a tree with balanced subtrees.
  protected def balance (c : Boolean) (l : RedBlackTree[V]) (k : Int) (v : Option[V]) (r : RedBlackTree[V]) : RedBlackTree[V] = {
    (c,l,k,v,r) match {
      case (B,T(R,T(R,a,xK,xV,b),yK,yV,c),zK,zV,d) => T(R,T(B,a,xK,xV,b),yK,yV,T(B,c,zK,zV,d))
      case (B,T(R,a,xK,xV,T(R,b,yK,yV,c)),zK,zV,d) => T(R,T(B,a,xK,xV,b),yK,yV,T(B,c,zK,zV,d))
      case (B,a,xK,xV,T(R,T(R,b,yK,yV,c),zK,zV,d)) => T(R,T(B,a,xK,xV,b),yK,yV,T(B,c,zK,zV,d))
      case (B,a,xK,xV,T(R,b,yK,yV,T(R,c,zK,zV,d))) => T(R,T(B,a,xK,xV,b),yK,yV,T(B,c,zK,zV,d))
      case (c,a,xK,xV,b) => T(c,a,xK,xV,b)
    }
  }

  // modWith: Helper method; top node could be red.
  protected[rb] def modWith (k : Int, f : (Int, Option[V]) => Option[V]) : RedBlackTree[V]

  // modifiedWith: Insert, update and delete all in one.
  def modifiedWith (k : Int, f : (Int, Option[V]) => Option[V]) : RedBlackTree[V] =
    blacken(modWith(k,f))

  // get: Retrieve a value for a key.
  def get(k : Int) : Option[V]

  def apply(k:Int):Option[V] = get(k)

  def contains(k:Int):Boolean

  def getBiggestLowerOrEqual(k:Int):Option[(Int,V)]

  protected[rb] def getBiggestLowerAcc(k:Int, bestoFar:(Int,V)):Option[(Int,V)]

  def getSmallestBiggerOrEqual(k:Int):Option[(Int,V)]

  def getSmallest:Option[(Int,V)] = getSmallestBiggerOrEqual(Int.MinValue)

  protected[rb] def getSmallestBiggerAcc(k:Int, bestSoFar:(Int,V)):Option[(Int,V)]

  // insert: Insert a value at a key.
  def insert (k : Int, v : V) = modifiedWith (k, (_,_) => Some(v))

  // remove: Delete a key.
  //TODO: dos not actually deletes the key!!! sets NONE to the value, the tree should be pruned, actually.
  def remove (k : Int) = modifiedWith (k, (_,_) => None)

  def size:Int
  def isEmpty:Boolean

  def values:List[V] = valuesAcc(List.empty)
  protected [rb] def valuesAcc(valuesAfter:List[V]):List[V]

  def positionOf(k: Int):Option[RBPosition[V]] = positionOfAcc(k:Int,null)
  protected[rb] def positionOfAcc(k:Int,positionAcc:QList[(T[V],Boolean)]):Option[RBPosition[V]]
}


// A leaf node.
private case class L[V]() extends RedBlackTree[V]  {

  def get(k : Int) : Option[V] = None


  override def contains(k : Int) : Boolean = false

  override protected[rb] def modWith (k : Int, f : (Int, Option[V]) => Option[V]) : RedBlackTree[V] = {
    T(R, this, k, f(k,None), this)
  }

  def getBiggestLowerOrEqual(k:Int):Option[(Int,V)] = None

  override protected[rb] def getBiggestLowerAcc(k:Int, bestSoFar:(Int,V)) = Some(bestSoFar)

  override def getSmallestBiggerOrEqual(k: Int):Option[(Int,V)] = None

  override protected[rb] def getSmallestBiggerAcc(k: Int, bestSoFar: (Int, V)) = Some(bestSoFar)

  override def size: Int = 0
  override def isEmpty = true

  protected [rb] def valuesAcc(valuesAfter:List[V]):List[V] = valuesAfter

  protected[rb] override def positionOfAcc(k : Int, positionAcc : QList[(T[V],Boolean)]) : Option[RBPosition[V]] = None
}


// A tree node.
private case class T[V](c : Boolean, l : RedBlackTree[V], k : Int, v : Option[V], r : RedBlackTree[V]) extends RedBlackTree[V] {
  val size = l.size + r.size + 1
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

  def getBiggestLowerOrEqual(k:Int):Option[(Int,V)] = {
    if (k < this.k) l.getBiggestLowerOrEqual(k)
    else if (this.k < k) r.getBiggestLowerAcc(k,(this.k,v.head))
    else Some(k,v.head)
  }

  override protected[rb] def getBiggestLowerAcc(k:Int, bestSoFar:(Int,V)):Option[(Int,V)] = {
    if (k < this.k) l.getBiggestLowerAcc(k, bestSoFar)
    else if (this.k < k) r.getBiggestLowerAcc(k, (this.k, v.head))
    else Some(k,v.head)
  }

  override def getSmallestBiggerOrEqual(k: Int):Option[(Int,V)] = {
    if (k < this.k) l.getSmallestBiggerAcc(k, (this.k, v.head))
    else if (this.k < k) r.getSmallestBiggerOrEqual(k)
    else Some(k,v.head)
  }

  override protected[rb] def getSmallestBiggerAcc(k: Int, bestSoFar: (Int,V)):Option[(Int,V)] = {
    if (k < this.k) l.getSmallestBiggerAcc(k, (this.k, v.head))
    else if (this.k < k) r.getSmallestBiggerAcc(k, bestSoFar)
    else Some(k,v.head)
  }

  override protected[rb] def modWith (k : Int, f : (Int, Option[V]) => Option[V]) : RedBlackTree[V] = {
    if (k <  this.k) balance (c) (l.modWith(k,f)) (this.k) (this.v) (r)
    else if (k == this.k) {
      f(this.k, this.v) match{
        case None =>
          if(l.isEmpty) r
          else if (r.isEmpty) l
          else{
            val (k,v) = r.getSmallest.head
            T(c, l, k, Some(v), r.remove(k))
          }
        case x => T(c, l, k, x, r)
      }
    }else {
      balance(c)(l)(this.k)(this.v)(r.modWith(k, f))
    }
  }

  override protected[rb] def valuesAcc(valuesAfter : List[V]) : List[V] = l.valuesAcc(v.head :: r.valuesAcc(valuesAfter))

  protected[rb] override def positionOfAcc(k : Int, positionAcc : QList[(T[V],Boolean)]) : Option[RBPosition[V]] = {
    if (k < this.k) l.positionOfAcc(k, QList((this,false),positionAcc))
    else if (k > this.k) r.positionOfAcc(k, QList((this,true),positionAcc))
    else Some(new RBPosition[V](QList((this,true),positionAcc)))
  }

  def hasLeft:Boolean = l.isInstanceOf[T[V]]
  def hasRight:Boolean = r.isInstanceOf[T[V]]
}

// A helper object.
object RedBlackTree {

  // empty: Converts an orderable type into an empty RBMap.
  def empty[V] : RedBlackTree[V] = L()

  // apply: Assumes an implicit conversion.
  def apply[V](args : (Int,V)*) : RedBlackTree[V] = {
    var currentMap : RedBlackTree[V] = L()
    for ((k,v) <- args) {
      currentMap = currentMap.insert(k,v)
    }
    currentMap
  }
}

//le booléen: true le noeud a déjà été montré (dans un parcour gauche à droite)
class RBPosition[V](position:QList[(T[V],Boolean)]){
  def key:Int = position.head._1.k
  def value:V = position.head._1.v.head
  def next:Option[RBPosition[V]] = {

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
    else Some(new RBPosition[V](newStack))
  }

  def prev:Option[RBPosition[V]] = {

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
      case t : T[V] => descendToRightMost(QList((t,false),position))
      case _ => unstack1(position)
    }

    if(newStack == null) None
    else Some(new RBPosition[V](newStack))
  }
}
