package oscar.cbls.test.algo

import oscar.cbls.algo.magicArray.MagicIntArrayStacked

object TestMagicIntArray extends App{

  val a = new MagicIntArrayStacked(maxLevel = 3, initVal = (w => w), size = 10)

  println(a)
  a(0) = 4
  require(a(0) == 4)
  a(1) = 2
  require(a(1) == 2)
  a.pushLevel()

  require(a(0) == 4)
  require(a(1) == 2)

  a(0) = 5
  require(a(0) == 5)
  println(a)

  a.popLevel(true)

  require(a(0) == 4)

  println(a)

}