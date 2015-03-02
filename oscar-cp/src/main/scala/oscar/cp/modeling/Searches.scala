package oscar.cp.modeling

import oscar.cp.core.variables.CPIntVar

trait Searches {

  def minDom(x: CPIntVar): Int = x.size
  
  def minDom(x: Array[CPIntVar]): Int => Int = i => x(i).size

  def minRegret(x: CPIntVar): Int = x.max - x.min
  
  def minRegret(x: Array[CPIntVar]): Int => Int = i => x(i).max - x(i).min

  def minDomMaxDegree(x: CPIntVar): (Int, Int) = (x.size, -x.constraintDegree)
  
  def minDomMaxDegree(x: Array[CPIntVar]): Int => (Int,Int) = (i => (x(i).size, -x(i).constraintDegree))
  
  def minVar(x: CPIntVar): Int = 1
  
  def minVar(x: Array[CPIntVar]): Int => Int = i => i
  
  def maxDegree(x: CPIntVar): Int = -x.constraintDegree
  
  def maxDegree(x: Array[CPIntVar]) = (i : Int) => -x(i).constraintDegree
  
  def minVal(x: CPIntVar): Int = x.min
  
  def minVal(x: Array[CPIntVar]) = (i : Int) => x(i).min
  
  def maxVal(x: CPIntVar): Int = x.max
  
  def maxVal(x: Array[CPIntVar]) = (i : Int) => x(i).max
  
  def minValminVal(x: CPIntVar): (Int, Int) = (x.min, x.min)
}