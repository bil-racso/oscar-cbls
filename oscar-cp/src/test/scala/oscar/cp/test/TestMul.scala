package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.cp.core.CPStore
import oscar.cp.core.CPVarInt
import oscar.cp.constraints.WeightedSum
import oscar.cp.constraints.MulCte
import oscar.cp.constraints.Eq
import oscar.cp.constraints.LeEq
import oscar.cp.constraints.Gr
import oscar.cp.constraints.MulVar
import oscar.cp.constraints.DiffVal

class TestMul extends FunSuite with ShouldMatchers {

  test("test Mul 1") {
    implicit val s = new CPStore()
    val x = CPVarInt(0, 5)
    val y = x.mul(-2)
    s.post(new Gr(y, -10))
    assert(x.max == 4)
  }

  test("test Mul 2") {
    implicit val s = new CPStore()
    val x = CPVarInt(2, 5)
    val y = CPVarInt(10, 10)
    s.post(new MulCte(x, 3, y))

    assert(s.isFailed())
  }

  test("test Mul 3") {
    implicit val s = new CPStore()
    val x = CPVarInt(2, 5)
    val z = CPVarInt(10, 12)
    s.post(new MulCte(x, 3, z))
    assert(!s.isFailed())
    assert(x.isBound && x.value == 4)
  }

  test("test Mul 4") {
    implicit val s = new CPStore()
    val x = CPVarInt(2, 5)
    val z = CPVarInt(9, 12)
    s.post(new MulCte(x, 3, z))
    s.post(new LeEq(z, 11))
    assert(!s.isFailed())
    assert(x.isBound && x.value == 3)
  }

  test("test Mul 5") {
    implicit val s = new CPStore()
    val x = CPVarInt(2, 5)
    val y = CPVarInt(10, 10)
    //s.post(new MulCte(x,3,y))
    s.post(new WeightedSum(Array(3), Array(x), y))
    assert(s.isFailed())

    assert(true)
  }

  test("test Mul 6") {
    implicit val cp = new CPStore()
    val x = CPVarInt(-1, 4)
    val y = CPVarInt(-1, 1)
    val z = CPVarInt(0, 0)

    cp.post(new MulVar(x, y, z))
    cp.post(new DiffVal(y, 0))

    assert(!cp.isFailed())
    assert(x.isBound && x.value == 0)
  }

  test("test Mul 7") {
    implicit val cp = new CPStore()
    val x = CPVarInt(-1, 4)
    val y = CPVarInt(-1, 1)
    val z = CPVarInt(0, 1)

    cp.post(new MulVar(x, y, z))
    cp.post(new DiffVal(z, 0))

    assert(!cp.isFailed())
    assert(!x.hasValue(0))
    assert(!y.hasValue(0))
    assert(x.min >= -1 && x.max <= 1)
    assert(y.min >= -1 && y.max <= 1)
  }

  test("test Mul 8") {

    //could prune better x1

    implicit val cp = new CPStore()
    val x = CPVarInt(-4, 4)
    val y = CPVarInt(-1, 1)
    val z = CPVarInt(-1, 1)

    cp.post(new MulVar(x, y, z))
    cp.post(new DiffVal(y, 0))

    //System.out.println(x+" "+y+" "+z)
    //should prune better x since y!=0.

    //System.out.println(x.hasValue(0))

  }

  test("test Mul 9") {
    //could prune better x1 and y1

    implicit val cp = new CPStore()
    val x = CPVarInt(-4, 4)
    val y = CPVarInt(-1, 1)
    val z = CPVarInt(-1, 1)

    cp.post(new MulVar(x, y, z))
    cp.post(new DiffVal(z, 0))
  }

  test("test Mul 10") {

    implicit val cp = new CPStore()
    val x = CPVarInt(0, 4)
    val y = CPVarInt(-3, 1)
    val z = CPVarInt(-1, 1)

    cp.post(new MulVar(x, y, z))
    cp.post(new LeEq(y, 0))

    assert(!z.hasValue(1))
  }

  test("test Mul 11") {

    implicit val cp = new CPStore()
    val x = CPVarInt(0, 4)
    val y = CPVarInt(-3, 0)
    val z = CPVarInt(0, 2)

    cp.post(new MulVar(x, y, z))

    assert(x.max == 4)
    assert(x.min == 0)
    assert(z.value == 0)

  }

  test("test Mul 12") {

    implicit val cp = new CPStore()
    val x = CPVarInt(0, 4)
    val y = CPVarInt(-3, 0)
    val z = CPVarInt(0, 2)

    cp.post(new MulVar(x, y, z))
    cp.post(new LeEq(y, -1))

    assert(x.value == 0)
    assert(z.value == 0)

  }

  test("test Mul 13") {

    implicit val cp = new CPStore()
    val x = CPVarInt(1, 4)
    val y = CPVarInt(-3, 0)
    val z = CPVarInt(-2, 2)

    cp.post(new MulVar(x, y, z))
    cp.post(new LeEq(y, -1))

    assert(x.max == 2)
    assert(y.min == -2)
    assert(z.max == -1)

  }

  test("test Mul 14") {

    implicit val cp = new CPStore()
    val s = CPVarInt(1, 30)
    val nb = CPVarInt(10506, 19596)
    val tmp = CPVarInt(351, 900)

    cp.post(new MulVar(tmp, s, nb))
    cp.post(new MulVar(s, s, tmp))
  }

  test("test Mul 15") {

    implicit val cp = new CPStore()
    val x = CPVarInt(1, 10)
    val y = CPVarInt(Set(50, 70))
    val z = CPVarInt(100, 100)

    cp.post(new MulVar(x, y, z))

    assert(x.isBoundTo(2))
    assert(y.isBoundTo(50))

  }

  test("test Mul 16") {

    implicit val cp = new CPStore()
    val x = CPVarInt(0, 10)
    val y = CPVarInt(Set(50, 70))
    val z = CPVarInt(100, 100)

    cp.post(new MulVar(x, y, z))

    assert(x.isBoundTo(2))
    assert(y.isBoundTo(50))

  }

  test("test Mul 18") {
    implicit val s = new CPStore()
    val x = CPVarInt(-5, 5)
    val y = CPVarInt(-5, 16)
    s.post(new Eq(x.mul(x), y)) // should detect it is a square constraint
    assert(!s.isFailed())
    assert(x.min == -4)
    assert(x.max == 4)
    assert(y.max == 16)
    assert(y.min == 0)
  }

  test("test Mul 19") {
    implicit val s = new CPStore()
    val x = CPVarInt(6, 43986624)
    val y = CPVarInt(4, 355)
    var z = CPVarInt(711, 711)
    //s.post(new MulVar(x, y, z))
    System.out.println("hello")
    z = x.mul(y)
    System.out.println("z:" + z)
    s.post(new Eq(z, CPVarInt(711))) // should detect it is a square constraint
    assert(!s.isFailed())

  }
}
