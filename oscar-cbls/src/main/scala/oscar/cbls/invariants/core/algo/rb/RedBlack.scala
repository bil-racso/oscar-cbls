package oscar.cbls.invariants.core.algo.rb

import oscar.cbls.invariants.core.algo.quick.QList

/*** Okasaki-style red-black tree maps. ***/

private object RedBlackTreeLib{
  val R = true
  val B = false

  // blacken: Turn a node black.
  def blacken[V] (n : RedBlackTree[V])  : RedBlackTree[V] = {
    n match {
      case L() => n
      case T(_,l,k,v,r) => T(B,l,k,v,r)
    }
  }

  // balance: Balance a tree with balanced subtrees.
  def balance[V] (c : Boolean) (l : RedBlackTree[V]) (k : Int) (v : Option[V]) (r : RedBlackTree[V]) : RedBlackTree[V] = {
    (c,l,k,v,r) match {
      case (B,T(R,T(R,a,xK,xV,b),yK,yV,c),zK,zV,d) => T(R,T(B,a,xK,xV,b),yK,yV,T(B,c,zK,zV,d))
      case (B,T(R,a,xK,xV,T(R,b,yK,yV,c)),zK,zV,d) => T(R,T(B,a,xK,xV,b),yK,yV,T(B,c,zK,zV,d))
      case (B,a,xK,xV,T(R,T(R,b,yK,yV,c),zK,zV,d)) => T(R,T(B,a,xK,xV,b),yK,yV,T(B,c,zK,zV,d))
      case (B,a,xK,xV,T(R,b,yK,yV,T(R,c,zK,zV,d))) => T(R,T(B,a,xK,xV,b),yK,yV,T(B,c,zK,zV,d))
      case (c,a,xK,xV,b) => T(c,a,xK,xV,b)
    }
  }
}

import RedBlackTreeLib._

//must use trait here because of specialization, so we ensure that this trait is compiled into a java interface by avoiding method code altogether. in the trait.
//as a consequence, there are duplicates in the classes implementing this trait.
trait RedBlackTree[@specialized(Int) V]{

  /* We could have required that K be <: Ordered[K], but this is
  actually less general than requiring an implicit parameter that can
  convert K into an Ordered[K].

  For example, Int is not compatible with Ordered[Int].  This would
  make it unusable with this map; however, it's simple to define an
  injector from Int into Ordered[Int].

  In fact, the standard prelude already defines just such an implicit:
  intWrapper. */

  // modWith: Helper method; top node could be red.
  protected[rb] def modWith (k : Int, f : (Int, Option[V]) => Option[V]) : RedBlackTree[V]

  // get: Retrieve a value for a key.
  def get(k : Int) : Option[V]

  def contains(k:Int):Boolean

  def biggestLowerOrEqual(k:Int):Option[(Int,V)]

  protected[rb] def getBiggestLowerAcc(k:Int, bestoFar:(Int,V)):Option[(Int,V)]

  def smallestBiggerOrEqual(k:Int):Option[(Int,V)]

  def smallest:Option[(Int,V)]

  def biggest:Option[(Int,V)]

  def biggestPosition:Option[RBPosition[V]]

  def smallestPosition:Option[RBPosition[V]]

  protected[rb] def getSmallestBiggerAcc(k:Int, bestSoFar:(Int,V)):Option[(Int,V)]

  // insert: Insert a value at a key.
  def insert (k : Int, v : V) : RedBlackTree[V]

  // remove: Delete a key.
  def remove (k : Int) : RedBlackTree[V]

  def size:Int
  def isEmpty:Boolean

  def values:List[V]
  protected [rb] def valuesAcc(valuesAfter:List[V]):List[V]
  def content:List[(Int,V)]
  protected [rb] def contentAcc(valuesAfter:List[(Int,V)]):List[(Int,V)]

  def positionOf(k: Int):Option[RBPosition[V]]
  protected[rb] def positionOfAcc(k:Int,positionAcc:QList[(T[V],Boolean)]):Option[RBPosition[V]]
}

// A leaf node.
case class L[@specialized(Int) V]() extends RedBlackTree[V]  {


  def get(k : Int) : Option[V] = None

  override def contains(k : Int) : Boolean = false

  override protected[rb] def modWith (k : Int, f : (Int, Option[V]) => Option[V]) : RedBlackTree[V] = {
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

  protected[rb] override def positionOfAcc(k : Int, positionAcc : QList[(T[V],Boolean)]) : Option[RBPosition[V]] = None


  //duplicates
  def values:List[V] = valuesAcc(List.empty)

  def content:List[(Int,V)] = contentAcc(List.empty)

  override def positionOf(k: Int):Option[RBPosition[V]] = positionOfAcc(k:Int,null)

  // insert: Insert a value at a key.
  override def insert (k : Int, v : V) = blacken(modWith(k, (_,_) => Some(v)))

  // remove: Delete a key.
  override def remove (k : Int) = blacken(modWith(k, (_,_) => None))

  override def smallest:Option[(Int,V)] = smallestBiggerOrEqual(Int.MinValue)

  override def biggest:Option[(Int,V)] = biggestLowerOrEqual(Int.MaxValue)

  override def biggestPosition:Option[RBPosition[V]] = None

  override def smallestPosition:Option[RBPosition[V]] = None
}

// A tree node.
case class T[@specialized(Int) V](c : Boolean, l : RedBlackTree[V], k : Int, v : Option[V], r : RedBlackTree[V]) extends RedBlackTree[V] {
  val mSize = l.size + r.size + 1
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

  override protected[rb] def modWith (k : Int, f : (Int, Option[V]) => Option[V]) : RedBlackTree[V] = {
    if (k <  this.k) balance (c) (l.modWith(k,f)) (this.k) (this.v) (r)
    else if (k == this.k) {
      f(this.k, this.v) match{
        case None =>
          if(l.isEmpty) r
          else if (r.isEmpty) l
          else{
            val (k,v) = r.smallest.head
            T(c, l, k, Some(v), r.remove(k))
          }
        case x => T(c, l, k, x, r)
      }
    }else {
      balance(c)(l)(this.k)(this.v)(r.modWith(k, f))
    }
  }

  override protected[rb] def valuesAcc(valuesAfter : List[V]) : List[V] = l.valuesAcc(v.head :: r.valuesAcc(valuesAfter))

  protected [rb] def contentAcc(valuesAfter:List[(Int,V)]):List[(Int,V)] = l.contentAcc((k,v.head) :: r.contentAcc(valuesAfter))

  protected[rb] override def positionOfAcc(k : Int, positionAcc : QList[(T[V],Boolean)]) : Option[RBPosition[V]] = {
    if (k < this.k) l.positionOfAcc(k, QList((this,false),positionAcc))
    else if (k > this.k) r.positionOfAcc(k, QList((this,true),positionAcc))
    else Some(new RBPosition[V](QList((this,true),positionAcc)))
  }

  def hasLeft:Boolean = l.isInstanceOf[T[V]]
  def hasRight:Boolean = r.isInstanceOf[T[V]]

  //duplicates
  def values:List[V] = valuesAcc(List.empty)


  override def content : List[(Int, V)] = contentAcc(List.empty)

  override def positionOf(k: Int):Option[RBPosition[V]] = positionOfAcc(k:Int,null)

  // insert: Insert a value at a key.
  override def insert (k : Int, v : V) = blacken(modWith(k, (_,_) => Some(v)))

  // remove: Delete a key.
  override def remove (k : Int) = blacken(modWith(k, (_,_) => None))

  override def smallest:Option[(Int,V)] = smallestBiggerOrEqual(Int.MinValue)

  override def biggest:Option[(Int,V)] = biggestLowerOrEqual(Int.MaxValue)

  override def biggestPosition:Option[RBPosition[V]] = {
    biggestLowerOrEqual(Int.MaxValue) match{
      case Some((k,_)) => positionOf(k)
      case None => None
    }
  }

  override def smallestPosition:Option[RBPosition[V]] = {
    smallestBiggerOrEqual(Int.MinValue) match{
      case Some((k,_)) => positionOf(k)
      case None => None
    }
  }

}

// A helper object.
object RedBlackTree {

  // empty: Converts an orderable type into an empty RBMap.
  def empty[@specialized(Int) V] : RedBlackTree[V] = L[V]()

  // apply: Assumes an implicit conversion.
  def apply[@specialized(Int) V](args : (Int,V)*) : RedBlackTree[V] = {
    var currentMap : RedBlackTree[V] = L()
    for ((k,v) <- args) {
      currentMap = currentMap.insert(k,v)
    }
    currentMap
  }

  //TODO: check this
  def makeFromSorted[@specialized(Int) V](args:Iterable [(Int,V)]): RedBlackTree[V] = {
    //root is to be black, beside alternate red and black
    val a = args.toArray
    myMakeFromSorted(a,0,a.length-1,false)
  }

  private def myMakeFromSorted[@specialized(Int) V](args:Array[(Int,V)],fromIncluded:Int,toIncluded:Int,targetIsRed:Boolean): RedBlackTree[V] = {
    //root is to be black, beside alternate red and black
    if(fromIncluded == toIncluded){
      val(key,value) = args(fromIncluded)
      T(targetIsRed, L(),  key, Some(value), L())
    }else if (fromIncluded + 1 == toIncluded) {
      val(keyL,valueL) = args(fromIncluded)
      val(keyH,valueH) = args(toIncluded)
      T(targetIsRed, T(!targetIsRed, L(),  keyL, Some(valueL), L()),  keyH, Some(valueH), L())
    }else{
      //there is a midlde point
      val middlePoint = (fromIncluded + toIncluded)/2
      val left = myMakeFromSorted(args,fromIncluded,middlePoint-1,!targetIsRed)
      val right = myMakeFromSorted(args,middlePoint+1,toIncluded,!targetIsRed)
      val(key,value) = args(middlePoint)
      T(targetIsRed, left,  key, Some(value), right)
    }
  }
}

//le booléen: true le noeud a déjà été montré (dans un parcour gauche à droite)
class RBPosition[@specialized(Int) V](position:QList[(T[V],Boolean)]){
  def key:Int = position.head._1.k
  def value:V = position.head._1.v.head

  override def toString : String = "RBPosition(key:" + key + " value:" + value + " stack:" + position.toList + ")"

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
      case t : T[V] => descendToRightMost(QList((t,true),QList((position.head._1,false),position.tail)))
      case _ => unstack1(position.tail)
    }

    if(newStack == null) None
    else {
      assert(new RBPosition[V](newStack).next.head.key == this.key, "prev.next.key != this.key; this:" + this + " prev:" + new RBPosition[V](newStack))
      Some(new RBPosition[V](newStack))
    }
  }
}
