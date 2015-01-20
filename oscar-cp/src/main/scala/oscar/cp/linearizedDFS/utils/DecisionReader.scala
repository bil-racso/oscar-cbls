package oscar.cp.linearizedDFS.utils

import oscar.cp.linearizedDFS._
import oscar.cp.linearizedDFS.{Pop, Push, Decision}
import scala.io.Source
import oscar.cp.core.variables.CPIntVar

/**
 * Created by saschavancauwelaert on 12/12/14.
 */
object DecisionReader {

  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  def decisionsFromFile(url : String, variables : Seq[CPIntVar]): Array[Decision] = {
    val store = variables(0).store
    val nameToVarMap = variables map {v => (v.name,v)} toMap

    val AssignPattern = ("(.+)(" + Decision.assign + ")(\\d+)").r
    val RemovePattern = (".+" + Decision.`remove` + ".+").r

    Source.fromFile(url).getLines map { l => l match {
      case Decision.`push` => Push(store)
      case Decision.`pop` => Pop(store)
      case Decision.`fail` => Fail(store)
      case Decision.`propagate` => Propagate(store)
      case AssignPattern(variableName,Decision.assign,value) => Assign(nameToVarMap(variableName),value toInt)
      case RemovePattern(variableName,Decision.remove,value) => Remove(nameToVarMap(variableName),value toInt)
      case _ => throw new Exception("Unsupported decision : " + l)
    }
    } toArray
  }
}
