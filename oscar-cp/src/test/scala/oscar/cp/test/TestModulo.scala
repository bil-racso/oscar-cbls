package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.cp.core.CPStore
import oscar.cp.core.CPVarInt
import oscar.cp.constraints.DiffVal
import oscar.cp.constraints.Modulo
import java.util.Arrays

class TestModulo extends FunSuite with ShouldMatchers {

  test("test modulo 1") {
    implicit val cp = new CPStore()
    val x = CPVarInt(List.apply(0, 3, 9, 12))
    val y = CPVarInt(0, 5)
    cp.post(new Modulo(x, 3, y))
    assert(y.isBound)
    assert(y.value == 0)
  }

  test("test modulo 2") {
    implicit val cp = new CPStore()
    val x = CPVarInt(Set(0, 1, 6, 9, 12))
    val y = CPVarInt(0, 5)
    cp.post(new Modulo(x, 3, y))
    assert(y.size == 2)
    cp.post(new DiffVal(x, 1))
    assert(y.isBound)
    assert(y.value == 0)
  }

  test("test modulo 3") {
    implicit val cp = new CPStore()
    val x = CPVarInt(Set(0, 1, 6, 9, 12))
    val y = CPVarInt(0, 5)
    cp.post(new Modulo(x, 3, y))
    cp.post(new DiffVal(y, 0))
    assert(x.isBound)
    assert(x.value == 1)
  }

  test("test modulo 4") {
    implicit val cp = new CPStore()
    val x = CPVarInt(Set(0, 1, 6, 2, 9, 12))
    val y = CPVarInt(0, 5)
    cp.post(new Modulo(x, 3, y))
    cp.post(new DiffVal(y, 0))
    cp.post(new DiffVal(y, 2))
    assert(x.isBound)
    assert(x.value == 1)
    assert(y.value == 1)
  }

  test("test modulo 5") {
    implicit val cp = new CPStore()
    val x = CPVarInt(Set(0, -1, -6, -2, -9, -12))
    val y = CPVarInt(-5, 5)
    cp.post(new Modulo(x, 3, y))
    cp.post(new DiffVal(y, 0))
    cp.post(new DiffVal(y, -2))
    assert(x.isBound)
    assert(x.value == -1)
    assert(y.value == -1)
  }

  test("test modulo 6") {
    implicit implicit val cp = new CPStore()
    val x = CPVarInt(Set(-6, -3, -9, -12, 3, 6, 9, 12))
    val y = CPVarInt(-5, 5)
    cp.post(new Modulo(x, 3, y))
    assert(y.value == 0)
  }
}