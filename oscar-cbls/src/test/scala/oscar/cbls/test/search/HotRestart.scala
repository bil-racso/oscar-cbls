package oscar.cbls.test.search

import oscar.cbls.search.algo.InstrumentedRange

import scala.collection.immutable.SortedSet

/**
 * Created by rdl on 16/07/2014.
 */
object HotRestart extends App{

  println(new InstrumentedRange(0 to 9) startBy 5)


  val s:SortedSet[Int] = SortedSet(1, 2, 3, 4, 7, 8, 9)
  val it = s.iteratorFrom(7)
  while(it.hasNext) println("next:" + it.next())


  println(oscar.cbls.search.algo.HotRestart(s,0))

}
