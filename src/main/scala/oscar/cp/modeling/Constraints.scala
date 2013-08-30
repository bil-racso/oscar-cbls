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


package oscar.cp.modeling

import oscar.cp.constraints._
import oscar.cp.core._
import oscar.cp.scheduling._
import scala.collection._
import scala.collection.mutable.ArrayBuffer
import java.util.LinkedList
import scala.collection.immutable.Set

trait Constraints {

  /**
   * Bin-Packing Constraint linking the placement variables of sized items into bins with
   * the total size of the bins
   * @param x with x(i) is the bin where the item i is placed
   * @param w with w(i) is the size of item i
   * @param l with l(j) is the load of bin j
   * @return a binpacking constraint linking the variables in argument such that l[i] == sum,,j,, w[j]*(x[j]==i) for all bins i
   */
  def binPacking(x: IndexedSeq[CPVarInt], w: IndexedSeq[Int], l: IndexedSeq[CPVarInt]): Constraint = {
    return new BinPacking(x.toArray, w.toArray, l.toArray)
  }
  /**
   * @deprecated(use binPacking instead)
   */
  def binpacking(x: IndexedSeq[CPVarInt], w: IndexedSeq[Int], l: IndexedSeq[CPVarInt]) = binPacking(x, w, l)

  /**
   * Bin-Packing Constraint linking the placement variables of sized items into bins with
   * the total size of the bins
   * @param x with x(i) is the bin where the item i is placed
   * @param w with w(i) is the size of item i
   * @param l with l(j) is the load of bin j
   * @param c with c(j) is the cardinality of bin j (number of items)
   * @return a binpacking constraint linking the variables in argument such that l[i] == sum,,j,, w[j]*(x[j]==i) for all bins i and
   */
  def binPackingCardinality(x: IndexedSeq[CPVarInt], w: IndexedSeq[Int], l: IndexedSeq[CPVarInt], c: IndexedSeq[CPVarInt]): Constraint = {
    return new BinPackingFlow(x.toArray, w.toArray, l.toArray, c.toArray)
  }

  /**
   * Binary-Knapsack Constraint computing the total weight of items placed into a knapsack
   * @param x with x(i) == 1 if the item is selected, 0 otherwise
   * @param w with w(i) is the weight of item i
   * @param l the load of the knapsack
   * @return a binary-knapsack constraint linking the variables in argument such that W == sum,,j,, w[j]*x[i]
   */
  def binaryKnapsack(x: IndexedSeq[CPVarBool], w: IndexedSeq[Int], W: CPVarInt): Constraint = {
    return new BinaryKnapsack(x.toArray, w.toArray, W)
  }

  /**
   * Binary-Knapsack Constraint computing the total weight of items placed into a knapsack
   * @param x with x(i) == 1 if the item is selected, 0 otherwise
   * @param w with w(i) is the weight of item i
   * @param l the load of the knapsack
   * @return a binary-knapsack constraint linking the variables in argument such that W == sum,,j,, w[j]*x[i]
   */
  def binaryKnapsack(x: IndexedSeq[CPVarBool], w: IndexedSeq[Int], W: Int): Constraint = {
    return new BinaryKnapsack(x.toArray, w.toArray, CPVarInt(x(0).store, W))
  }

  /**
   * Binary-Knapsack Constraint computing the total profit of items placed into a capacitated knapsack
   * @param x with x(i) == 1 if the item is selected, 0 otherwise
   * @param p with p(i) >= 0 is the profit you get selecting item i
   * @param w with w(i) > 0 is the weight of item i into the knapsack
   * @param P the total profit of the knapsack
   * @param W the total weight of the knapsack
   * @return a binary-knapsack constraint linking the variables in argument such that P == sum,,j,, p[j]*x[i] and W == sum,,j,, w[j]*x[i]
   */
  def binaryKnapsack(x: IndexedSeq[CPVarBool], p: IndexedSeq[Int], w: IndexedSeq[Int], P: CPVarInt, W: CPVarInt): Knapsack = {
    return new Knapsack(x.toArray, p.toArray, w.toArray, P, W)
  }
  
  /**
   * atLeastNValue Constraint (Available Filtering: Weak, Strong)
   * @param vars an non empty array of variables
   * @return a constraint ensuring that nValue is the number of different valuesin vars
   */
  def atLeastNValue(vars: Iterable[CPVarInt], nValue: CPVarInt): Constraint = {
    return new AtLeastNValue(vars.toArray,nValue)
  }  

  /**
   * allDifferent Constraint (Available Filtering: Weak, Strong)
   * @param vars an non empty array of variables
   * @return a constraint ensure that no value occurs more than once in vars
   */
  def allDifferent(vars: CPVarInt*): Constraint = {
    return new AllDifferent(vars: _*)
  }

  def allDifferent(vars: Iterable[CPVarInt]): Constraint = {
    return allDifferent(vars.toArray: _*)
  }
  
  /**
   * minAssignment Constraint (Available Filtering: Medium)
   * @param vars an non empty array of variables, weights a n x n array (n = vars.size-1).
   * @return a constraint ensure that allDifferent(x) and sum(i) weights(i)(x(i)) <= cost 
   */  
  def minAssignment(x: Array[CPVarInt], weights: Array[Array[Int]], cost: CPVarInt): MinAssignment = {
    return new MinAssignment(x,weights,cost)
  } 

  /**
   * @param succ[i] is the successor of node i (also place i inside the domain of succ[i] if you want to allow it not to be part of the path
   * @param start start is the index of the first node on the path
   * @param end is the index of the last node on the path
   * @param length is the length of the path (number edges)
   *
   * Example:
   * succ [1, 3, 2, 5, 4, 0], start = 0, end = 5, length = 3 represents the path 0 -> 1 -> 3 -> 5
   * Notice that nodes that do not belong to the path, have them-self as successor and that
   * the successor of the last node of the path is the first node by convention
   * @author Pierre Schaus
   */

  //  def path(succ: Array[CPVarInt], start: CPVarInt, end: CPVarInt, length: CPVarInt): Constraint = {
  //    return new Path(succ,start,end,length)
  //  }

  /**
   * Circuit Constraint (Available Filtering: Weak, Strong)
   * @param vars an array of n variable with domains defined on (0..n-1)
   * @return a constraint enforcing a circuit representation where vars(i) represents
   *         the city visited after city i (no city is visited twice and there is no sub-tours).
   *
   */
  def circuit(vars: Array[CPVarInt]): Constraint = {
    return new Circuit(vars)
  }

  /**
   * Lexicographically Less or Equal Constraint
   * @param x an non empty array of variables x1, x2, x3 ... xn
   * @param y an array y1, y2, y3 ... yn (of the same size as x)
   * @return a constraint enforcing x LexLeq y i.e. :
   * 		 x1 < y1 or (x2,x3,...,xn) LexLeq (y2,y3,...,yn)
   */
  def lexLeq(x: Array[CPVarInt], y: Array[CPVarInt]): Constraint = {
    new LexLeq(x, y)
  }

  /**
   * Element Constraint, indexing an array of integers by a variable
   * @param tab an non empty array n integers
   * @param x an index variable with domain defined on (0..n-1)
   * @return a variable z linked to tab and x by the relation tab(x) == z
   */
  def element(tab: IndexedSeq[Int], x: CPVarInt, strength: CPPropagStrength = CPPropagStrength.Medium): CPVarInt = {
    val minval = tab.min
    val maxval = tab.max
    val z = CPVarInt(x.store, minval, maxval)
    x.store.post(new ElementCst(tab.toArray, x, z), strength)
    z
  }

  /**
   * Element Constraint, indexing an array of integers by a variable
   * @param tab an non empty array n integers
   * @param x an index variable with domain defined on (0..n-1)
   * @param z an integer variable
   * @return a constraints such that tab , x and z are linked by the relation tab(x) == z
   */
  def element(tab: IndexedSeq[Int], x: CPVarInt, z: CPVarInt): Constraint = {
    new ElementCst(tab.toArray, x, z)
  }

  /**
   * Element Constraint, indexing an array of integers by a variable
   * @param tab an non empty array n integers
   * @param x an index variable with domain defined on (0..n-1)
   * @param z an integer
   * @return a constraints such that tab, x and z are linked by the relation tab(x) == z
   */
  def element(tab: IndexedSeq[Int], x: CPVarInt, z: Int): Constraint = {
    new ElementCst(tab.toArray, x, CPVarInt(x.store, z, z))
  }

  /**
   * Element 2D Constraint, indexing an integer matrix by two index variables
   * @param matrix rectangle matrix of sizes n x m
   * @param i the first index variable (line index) with domain defined on (0..n-1)
   * @param j the second index variable (column index) with domain defined on (0..m-1)
   * @return a variable z linked to the arguments with the relation matrix(i)(j) == z
   */
  def element(matrix: Array[Array[Int]], i: CPVarInt, j: CPVarInt): CPVarInt = {
    val z = CPVarInt(i.store, matrix.flatten.min to matrix.flatten.max)
    val ok = i.store.post(new ElementCst2D(matrix, i, j, z))
    assert(ok != CPOutcome.Failure, { println("element on matrix, should not fail") })
    return z
  }

  /**
   * Element Constraint, indexing an array of variables by a variable
   * @param tab an non empty array n variables
   * @param x an index variable with domain defined on (0..n-1)
   * @return an integer variable z such that tab, x and z are linked by the relation tab(x) == z
   */
  def elementVar(tab: IndexedSeq[CPVarInt], x: CPVarInt, l: CPPropagStrength = Weak): CPVarInt = {
    val minval = (for (x <- tab) yield x.getMin) min
    val maxval = (for (x <- tab) yield x.getMax) max
    val z = CPVarInt(x.store, minval, maxval)
    x.store.add(new ElementVar(tab.map(_.asInstanceOf[CPVarInt]).toArray, x, z), l)
    z
  }

  /**
   * Element Constraint, indexing an array of variables by a variable
   * @param tab an non empty array n variables
   * @param x an index variable with domain defined on (0..n-1)
   * @param z an integer variable
   * @return a constraints such that tab , x and z are linked by the relation tab(x) == z
   */
  def elementVar(tab: IndexedSeq[CPVarInt], x: CPVarInt, z: CPVarInt): Constraint = {
    new ElementVar(tab.map(_.asInstanceOf[CPVarInt]).toArray, x, z)
  }

  /**
   * Element Constraint, indexing an array of variables by a variable
   * @param tab an non empty array n variables
   * @param x an index variable with domain defined on (0..n-1)
   * @param z an integer
   * @return a constraints such that tab, x and z are linked by the relation tab(x) == z
   */
  def elementVar(tab: IndexedSeq[CPVarInt], x: CPVarInt, z: Int): Constraint = {
    new ElementVar(tab.toArray, x, CPVarInt(x.s, z))
  }

  /**
   * Element Constraint, indexing an array of variables by a variable
   * @param tab an non empty array n variables
   * @param x an index variable with domain defined on (0..n-1)
   * @param z an integer
   * @return a constraints such that tab, x and z are linked by the relation tab(x) == z
   */
  def elementVar(tab: IndexedSeq[CPVarBool], x: CPVarInt, z: Boolean): Constraint = {
    val z_ = new CPVarBool(x.store, z)
    new ElementVar(tab.map(_.asInstanceOf[CPVarInt]).toArray, x, z_)
  }

  /**
   * Sum Constraint
   * @param vars a non empty array of n variables
   * @param s a variable representing the sum of vars
   * @return a constraint enforcing vars(0)+vars(1)+...+vars(n) = s
   */
  def sum(vars: Array[CPVarInt], s: CPVarInt): Constraint = {
    /*
    var x = vars
    while (x.size > 2) {
      //System.err.println("sum"+x.size)
      val y = x.sliding(2, 2).toArray
      x = y.map{ arg =>
      	if (arg.size == 2) arg(0)+arg(1)
      	else arg(0)
      }.reverse
    }
    if (x.size == 2) new BinarySum(x(0),x(1),s)
    else x(0) == s
    */
    
    new oscar.cp.constraints.Sum2(vars, s)
  }

  /**
   * Sum Constraint
   * @param vars a non empty array of n variables
   * @return a variable representing vars(0)+vars(1)+...+vars(n)
   */
  def sum(vars: Iterable[CPVarInt]): CPVarInt = {
    val x = vars.toArray
    val minVal = (0 /: vars) {
      (sum, v) => sum + v.getMin
    }
    val maxVal = (0 /: vars) {
      (sum, v) => sum + v.getMax
    }
    val s = CPVarInt(x(0).store, minVal, maxVal)
    x(0).store.post(sum(x, s))
    s
  }

  /**
   * Sum Constraint
   * @param indexes
   * @param f a function mapping to a CPVarInt
   * @return a variable representing vars(f(i0))+vars(f(i1))+...+vars(f(in)) with i0, i1...,in the indexes
   */
  def sum[A](indexes: Iterable[A])(f: A => CPVarInt): CPVarInt = sum(indexes map f)

  /**
   * Sum Constraint
   * @param indexes1 a first iterable
   * @param indexes2 a second iterable
   * @param f a function mapping A,B to a variable with A from indexes1 and B from indexes2
   * @return a variable that is the sum of f(A,B) over each (A, B) in (indexes x indexes2)
   */
  def sum[A, B](indexes1: Iterable[A], indexes2: Iterable[B])(f: (A, B) => CPVarInt): CPVarInt = {
    sum(for (i <- indexes1; j <- indexes2) yield f(i, j))
  }

  /**
   * Sum Constraint
   * @param size of the first range of i
   * @param size of the second range of j
   * @param f a function mapping the indices i and j to a variable
   * @return a variable that is the sum of f(i, j) over each (i, j) in (0 until n1 x 0 until n2)
   */
  def sum(n1: Int, n2: Int)(f: (Int, Int) => CPVarInt): CPVarInt = {
    sum(0 until n1, 0 until n2)(f)
  }

  /**
   * Weighted Sum Constraint
   * @param indexes an iterable of index values
   * @param a function: i => (w_i, x_i) where i is an index from indexes
   * @return a variable S linked with the relation S = sum(i in indexes) (w_i * x_i)
   */
  def weightedSum[A](indexes: Iterable[A])(f: A => (Int, CPVarInt)): CPVarInt = {
    val (w,x) = (for (i <- indexes) yield f(i)).unzip
    weightedSum(w.toArray,x.toArray)
  }

  /**
   * Weighted Sum Constraint
   * @param indexes1 an iterable of index values
   * @param indexes2 an iterable of index values
   * @param a function: (i,j) => (w_ij, x_ij) where i is an index from indexes1, j is an index from indexes2
   * @return a variable S linked with the relation S = sum(i in indexes1,j in indexes2) (w_ij * x_ij)
   */
  def weightedSum[A, B](indexes1: Iterable[A], indexes2: Iterable[B])(f: (A, B) => (Int, CPVarInt)): CPVarInt = {
    val (w,x) = (for (i <- indexes1; j <- indexes2) yield f(i,j)).unzip
    weightedSum(w.toArray,x.toArray)
  }
  
  /**
   * Weighted Sum Constraint
   * @return y == sum(i)(w_i * x_i)
   */
  def weightedSum(w: Array[Int], x: Array[CPVarInt], y: CPVarInt): Constraint = {
    new WeightedSum2(w,x,y)
  }
  
  /**
   * Weighted Sum Constraint
   * @return y==sum(i)(w_i * x_i)
   */
  def weightedSum(w: Array[Int], x: Array[CPVarInt], y: Int): Constraint = {
    weightedSum(w,x,CPVarInt(x(0).s,y))
  }  

  /**
   * Weighted Sum Constraint
   * @return sum(i)(w_i * x_i)
   */
  def weightedSum(w: Array[Int], x: Array[CPVarInt]): CPVarInt = {
    val cp = x(0).s
    val m = w.zip(x).map{case(wi,xi) => if (wi < 0) wi*xi.max else wi*xi.min}.sum
    val M = w.zip(x).map{case(wi,xi) => if (wi < 0) wi*xi.min else wi*xi.max}.sum
    val y = CPVarInt(cp,m to M)
    cp.post(weightedSum(w,x,y))
    y
  }

  /**
   * Weighted Sum Constraint
   * @return sum(ij)(w_ij * x_ij)
   */
  def weightedSum(w: Array[Array[Int]], x: Array[Array[CPVarInt]]): CPVarInt = {
    weightedSum(w.flatten,x.flatten)
  }

  /**
   * Or (logical) Constraint
   * @param vars a non empty array of n variables
   * @return a variable that will be true if at least one variable of vars is true
   */
  def or(vars: Array[CPVarBool]): CPVarBool = {
    val z = new CPVarBool(vars(0).store)
    vars(0).store.post(new Or(vars, z))
    return (z)
  }

  /**
   * Or (logical) Constraint
   * @return a variable that will be true if at least one variable of f(i) is true for i in indexes
   */
  def or[A](indexes: Iterable[A])(f: A => CPVarBool): CPVarBool = {
    or((for (i <- indexes) yield f(i)).toArray)
  }

  def table(x: Array[CPVarInt], tuples: Array[Array[Int]]): Constraint = {
    //new TableSTR2(x,tuples)

    import oscar.cp.constraints.TableAC5TCRecomp
    val data = new TableData(x.size)
    tuples.foreach(t => data.add(t: _*))
    new oscar.cp.constraints.TableAC5TCRecomp(data, x: _*)

    /*
    val tab = new TableJava(x:_*)
    tuples.foreach(t => tab.addTupple(t:_*))
    return tab
  	*/
  }

  /**
   * Table Constraints for couples (constraint given in extension by enumerating valid assignments)
   * @param x1 first variable
   * @param x2 second variable
   * @param tuples a collection of coulples
   * @return a constraint enforcing that (x1,x2) is one of the couples given in tuples
   */
  def table(x1: CPVarInt, x2: CPVarInt, tuples: Iterable[(Int, Int)]): Constraint = {
    table(Array(x1, x2), tuples.map(t => Array(t._1, t._2)).toArray)
    //import oscar.cp.constraints.TableAC5TCRecomp
    //new oscar.cp.constraints.TableAC5TCRecomp(x1,x2,tuples)

  }

  /**
   * Table Constraints for triples (constraint given in extension by enumerating valid assignments)
   * @param x1 first variable
   * @param x2 second variable
   * @param x3 third variable
   * @param tuples a collection of triples
   * @return a constraint enforcing that (x1,x2,x3) is one of the triples given in tuples
   */
  def table(x1: CPVarInt, x2: CPVarInt, x3: CPVarInt, tuples: Iterable[(Int, Int, Int)]): Constraint = {
    table(Array(x1, x2, x3), tuples.map(t => Array(t._1, t._2, t._3)).toArray)
    //import oscar.cp.constraints.TableAC5TCRecomp
    //new oscar.cp.constraints.TableAC5TCRecomp(x1,x2,x3,tuples)
  }

  /**
   * Table Constraints for quadruples
   * @param x1 first variable
   * @param x2 second variable
   * @param x3 third variable
   * @param x4 fourth variable
   * @param tuples a collection of quadruples
   * @return a constraint enforcing that (x1,x2,x3,x4) is one of the quadruples given in tuples
   */
  def table(x1: CPVarInt, x2: CPVarInt, x3: CPVarInt, x4: CPVarInt, tuples: Iterable[(Int, Int, Int, Int)]): Constraint = {
    table(Array(x1, x2, x3, x4), tuples.map(t => Array(t._1, t._2, t._3, t._4)).toArray)
    //import oscar.cp.constraints.TableAC5TCRecomp
    //new oscar.cp.constraints.TableAC5TCRecomp(x1,x2,x3,x4,tuples)
  }

  /**
   * Table Constraints for quadruples
   * @param x1 first variable
   * @param x2 second variable
   * @param x3 third variable
   * @param x4 fourth variable
   * @param x5 fifth variable
   * @param tuples a collection of five-tuples
   * @return a constraint enforcing that (x1,x2,x3,x4,x5) is one of the five-tuples given in tuples
   */
  def table(x1: CPVarInt, x2: CPVarInt, x3: CPVarInt, x4: CPVarInt, x5: CPVarInt, tuples: Iterable[(Int, Int, Int, Int, Int)]): Constraint = {
    table(Array(x1, x2, x3, x4, x5), tuples.map(t => Array(t._1, t._2, t._3, t._4, t._5)).toArray)
    //import oscar.cp.constraints.TableAC5TCRecomp
    //new oscar.cp.constraints.TableAC5TCRecomp(x1,x2,x3,x4,tuples)
  }

  def modulo(x: CPVarInt, v: Int, y: CPVarInt): Constraint = {
    return new Modulo(x, v, y)
  }

  def modulo(x: CPVarInt, v: Int, y: Int): Constraint = {
    return new Modulo(x, v, CPVarInt(x.store, y))
  }
  
 
  /**
   * Among Constraint: n is the number of variables from x in set s.
   * @param n counter variable
   * @param x array of variables
   * @param s set of values
   * @return a constraint enforcing that n = #{ i | x(i) in s }
   */  
  def among(n: CPVarInt, x: IndexedSeq[CPVarInt], s: Set[Int]) = {
    new Among(n,x,s)
  }
  
  /**
   * AtLeast Constraint: at least n variables take their value in s
   * @param n counter variable
   * @param x array of variables
   * @param s set of values
   * @return a constraint enforcing that  #{ i | x(i) in s } >= n
   */  
  def atLeast(n: Int, x: IndexedSeq[CPVarInt], s: Set[Int]) = {
    among(CPVarInt(x(0).s,n,x.size),x,s)
  }

  /**
   * AtLeast Constraint: at least n variables equal to v
   * @param n counter variable
   * @param x array of variables
   * @param v a value
   * @return a constraint enforcing that  #{ i | x(i) = v } >= n
   */  
  def atLeast(n: Int, x: IndexedSeq[CPVarInt], v: Int): Constraint = atLeast(n,x,Set(v)) 
  
  /**
   * AtMost Constraint: at most n variables take their value in s
   * @param n counter variable
   * @param x array of variables
   * @param s set of values
   * @return a constraint enforcing that  #{ i | x(i) in s } <= n
   */  
  def atMost(n: Int, x: IndexedSeq[CPVarInt], s: Set[Int]) = {
    among(CPVarInt(x(0).s,0,n),x,s)
  }

  /**
   * AtMost Constraint: at least n variables equal to v
   * @param n counter variable
   * @param x array of variables
   * @param v a value
   * @return a constraint enforcing that  #{ i | x(i) = v } <= n
   */  
  def atMost(n: Int, x: IndexedSeq[CPVarInt], v: Int): Constraint = atMost(n,x,Set(v))
  
  /**
   * Count Constraint: n is the number of variables from x equal to y.
   * @param n is a counter variable
   * @param x is an array of variables
   * @param y is a variable
   * @return a constraint enforcing that n = #{ i | x(i) = y }
   */  
  def countEq(n: CPVarInt, x: IndexedSeq[CPVarInt], y: CPVarInt) = {
    new Count(n,x,y)
  }
  
  /**
   * Count Constraint: n is greater or equal to the number of variables from x equal to y.
   * @param n is a counter variable
   * @param x is an array of variables
   * @param y is a variable
   * @return a constraint enforcing that n >= #{ i | x(i) = y }
   */  
  def countGeq(n: CPVarInt, x: IndexedSeq[CPVarInt], y: CPVarInt) = {
    val c = CPVarInt(n.s,0 to x.size)
    val ok = n.s.post(n >= c)
    assert(ok != CPOutcome.Failure)
    new Count(c,x,y)
  }
  
  /**
   * Count Constraint: n is greater than the number of variables from x equal to y.
   * @param n is a counter variable
   * @param x is an array of variables
   * @param y is a variable
   * @return a constraint enforcing that n > #{ i | x(i) = y }
   */  
  def countGt(n: CPVarInt, x: IndexedSeq[CPVarInt], y: CPVarInt) = {
    val c = CPVarInt(n.s,0 to x.size)
    val ok = n.s.post(n > c)
    assert(ok != CPOutcome.Failure)
    new Count(c,x,y)
  }
  
  /**
   * Count Constraint: n is less or equal to the number of variables from x equal to y.
   * @param n is a counter variable
   * @param x is an array of variables
   * @param y is a variable
   * @return a constraint enforcing that n <= #{ i | x(i) = y }
   */  
  def countLeq(n: CPVarInt, x: IndexedSeq[CPVarInt], y: CPVarInt) = {
    val c = CPVarInt(n.s,0 to x.size)
    val ok = n.s.post(n <= c)
    assert(ok != CPOutcome.Failure)
    new Count(c,x,y)
  }
  
  /**
   * Count Constraint: n is less than the number of variables from x equal to y.
   * @param n is a counter variable
   * @param x is an array of variables
   * @param y is a variable
   * @return a constraint enforcing that n <= #{ i | x(i) = y }
   */  
  def countLt(n: CPVarInt, x: IndexedSeq[CPVarInt], y: CPVarInt) = {
    val c = CPVarInt(n.s,0 to x.size)
    val ok = n.s.post(n < c)
    assert(ok != CPOutcome.Failure)
    new Count(c,x,y)
  }  
  
  /**
   * Count Constraint: n is different to the number of variables from x equal to y.
   * @param n is a counter variable
   * @param x is an array of variables
   * @param y is a variable
   * @return a constraint enforcing that n != #{ i | x(i) = y }
   */  
  def countNeq(n: CPVarInt, x: IndexedSeq[CPVarInt], y: CPVarInt) = {
    val c = CPVarInt(n.s,0 to x.size)
    val ok = n.s.post(n != c)
    assert(ok != CPOutcome.Failure)
    new Count(c,x,y)
  }   
  
  /**
   * Global Cardinality Constraint: every value occurs at least min and at most max
   * @param x an non empty array of variables
   * @param values is the range of constrained values
   * @param min is the minimum number of occurrences for each value in the range values
   * @param max is the maximum number of occurrences for each value in the range values
   * @return a constraint such that each value in the range values occurs at least min and at most max times.
   */
  def gcc(x: Array[CPVarInt], values: Range, min: Int, max: Int): Constraint = {
    return new GCC(x, values.min, Array.fill(values.size)(min), Array.fill(values.size)(max))
  }

  /**
   * Global Cardinality Constraint: every value v occurs at least min(v) and at most max(v)
   * @param x an non empty array of variables
   * @param values is the range of constrained values
   * @param min is the minimum number of occurrences for each value in the range values
   * @param max is the maximum number of occurrences for each value in the range values
   * @return a constraint such that each value in the range values occurs at least min and at most max times.
   */
  def gcc(x: Array[CPVarInt], values: Range, min: Array[Int], max: Array[Int]): Constraint = {
    return new GCC(x, values.min, min, max)
  }

  /**
   * Soft Global Cardinality Constraint = gcc with a violation variable
   * @see Revisiting the Soft Global Cardinality Constraint, Pierre Schaus, Pascal Van Hentenryck, Alessandro Zanarini: CPAIOR 2010
   */
  def softGcc(x: Array[CPVarInt], values: Range, min: Array[Int], max: Array[Int], viol: CPVarInt): Constraint = {
    return new SoftGCC(x, values.min, min, max, viol)
  }

  /**
   * Global Cardinality Constraint with variable counters
   * @param x a non empty array of variables
   * @param valueOccurence is an array of pairs (v,o)
   *        where o is variable representing the number of occurrences of value v
   * @return a constraint such that for each (o,v) in valueOccurrence, o is the number of times the value v appears in x
   */
  def gcc(x: IndexedSeq[CPVarInt], valueOccurrence: Iterable[(Int,CPVarInt)]): Constraint = {
    def freshCard(): CPVarInt = CPVarInt(x.head.store, 0, x.size - 1)
    val sortedValOcc = valueOccurrence.toArray.sortWith((a, b) => a._1 <= b._1)
    val (v0,x0) = sortedValOcc(0)
    var values = Array(v0)
    var cardinalities = Array(x0)
    for (i <- 1 until sortedValOcc.length) {
      val (vi_1,xi_1) = sortedValOcc(i - 1)
      val (vi,xi) = sortedValOcc(i)
      for (v <- (vi_1 + 1) until vi) {
        values = values :+ v
        cardinalities = cardinalities :+ freshCard()
      }
      values = values :+ vi
      cardinalities = cardinalities :+ xi
    }
    new GCCVar(x.toArray, values(0), cardinalities)
  }

  
  def gcc(x: Array[CPVarInt], valueOccurrence: Array[(Int,CPVarInt)]): Constraint = {
    gcc(x.toIndexedSeq, valueOccurrence.toIterable)
  }

  // regular and automatons

  /**
   * @see stretchAutomaton(Array[CPVarInt],Int,Int,Iterable[Tuple2[Int, Int]])
   */
  def stretchAutomaton(vars: Array[CPVarInt], minStretch: Int, maxStretch: Int): Automaton = {
    stretchAutomaton(vars, minStretch, maxStretch, None)
  }

  /**
   * Builds an automaton restricting the number consecutive times the values appear.
   * A stretch is a consecutive number of a same value in vars for instance 112223335 start with a stretch of length 2 of value 1, followed by a stretch of length 3 with value 2,
   * followed by a stretch of length 3 of value 3 followed by a stretch of 1 of value 5.
   * @param vars an non empty array of variables
   * @param minStretch the minimum stretch length for any value
   * @param maxStretch the maximum stretch length for any value
   * @return an automaton
   */
  def stretchAutomaton(vars: Array[CPVarInt], minStretch: Int, maxStretch: Int, transitions: Iterable[Tuple2[Int, Int]]): Automaton = {
    val maxv = vars.map(x => x.getMax).max
    val minv = vars.map(x => x.getMin).min
    if (minv < 0) throw new RuntimeException("warning stretch automaton: some domains with <0 values, only >=0 values can be constrained")

    val minimumStretch = Array.tabulate(maxv + 1)(_ => minStretch)
    val maximumStretch = Array.tabulate(maxv + 1)(_ => maxStretch)

    stretchAutomaton(vars, minimumStretch, maximumStretch, transitions)
  }

  /**
   * @see stretchAutomaton(Array[CPVarInt],Int,Int,Iterable[Tuple2[Int, Int]])
   */
  def stretchAutomaton(vars: Array[CPVarInt], minStretch: Array[Int], maxStretch: Array[Int], transitions: Iterable[Tuple2[Int, Int]] = None): Automaton = {
    val maxv = vars.map(x => x.getMax).max
    val minv = vars.map(x => x.getMin).min
    if (minv < 0) throw new RuntimeException("warning stretch automaton: some domains with <0 values, only >=0 values can be constrained")

    if (transitions != None && !transitions.isEmpty) {
      var transiFrom = Array[Int]()
      var transiTo = Array[Int]()
      transitions.foreach(t => { transiFrom = transiFrom :+ t._1; transiTo = transiTo :+ t._2 })
      Stretch.getStretchAutomaton(vars, minStretch, maxStretch, transiFrom, transiTo)
    } else {
      Stretch.getStretchAutomaton(vars, minStretch, maxStretch)
    }
  }

  /**
   * Regular Constraint, ensuring that vars accepted by a context free grammar described by given automaton
   * @param vars an non empty array of variables, with domains belonging to the set of transitions of the automaton
   * @param automaton a deterministic automaton
   * @return a constraint ensuring that m is the maximum of variables in vars
   */
  def regular(vars: Array[CPVarInt], automaton: Automaton): Constraint = {
    new Regular(vars, automaton)
  }

  /**
   * Maximum Constraint
   * @param indexes
   * @param f function mapping each element from indexes to a variable
   * @return a fresh variable z linked to vars by a constraint such that z is the maximum of all variables f(A) for all A in indexes
   */
  def maximum[A](indexes: Iterable[A])(f: A => CPVarInt): CPVarInt = maximum(indexes map f)

  /**
   * Maximum Constraint
   * @param vars an non empty array of variables
   * @param m a variables representing the maximum of vars
   * @return a constraint ensuring that m is the maximum of variables in vars
   */
  def maximum(vars: Array[CPVarInt], m: CPVarInt): Constraint = {
    new Maximum(vars, m)
  }

  /**
   * Maximum Constraint
   * @param vars an non empty array of variables
   * @return a fresh variable z linked to vars by a constraint such that z is the maximum of all variables in vars
   */
  def maximum(vars: Iterable[CPVarInt]): CPVarInt = {
    val x = vars.toArray
    val cp = x(0).store
    val m = CPVarInt(cp, vars.map(_.min).max, vars.map(_.max).max)
    cp.add(maximum(x, m))
    m
  }

  /**
   * Minimum Constraint
   * @param indexes
   * @param f function mapping each element from indexes to a variable
   * @return a fresh variable z linked to vars by a constraint such that z is the minimum of all variables f(A) for all A in indexes
   */
  def minimum[A](indexes: Iterable[A])(f: A => CPVarInt): CPVarInt = minimum(indexes map f)

  /**
   * Minimum Constraint
   * @param vars an non empty array of variables
   * @param m a variables representing the maximum of vars
   * @return a constraint ensuring that m is the minimum of variables in vars
   */
  def minimum(vars: Array[CPVarInt], m: CPVarInt): Constraint = {
    new Minimum(vars, m)
  }

  /**
   * Minimum Constraint
   * @param vars an non empty array of variables
   * @return a fresh variable z linked to vars by a constraint such that z is the minimum of all variables in vars
   */
  def minimum(vars: Iterable[CPVarInt]): CPVarInt = {
    val x = vars.toArray
    val cp = x(0).store
    val m = CPVarInt(cp, vars.map(_.min).min, vars.map(_.max).min)
    cp.add(minimum(x, m))
    m
  }

  /**
   * Constraint enforcing n * sum,,i,, |x[i]-s/n| <= nd and sum,,i,, x[i] = s <br>
   * Note that this constraint is very similar spread.
   * @param x
   * @param s
   * @param nd
   */
  def deviation(x: Iterable[CPVarInt], s: Int, nd: CPVarInt): Constraint = {
    new Deviation(x.toArray, s, nd)
  }

  /**
   * Constraint enforcing sum,,i,, x[i]^2^ <= s2 and sum,,i,, x[i] = s <br>
   * Note that this constraint is very similar deviation.
   * @param x
   * @param s
   * @param s2
   */
  def spread(x: Iterable[CPVarInt], s: Int, s2: CPVarInt): Constraint = {
    new Spread(x.toArray, s, s2, true)
  }

  /**
   * Let n = x.size-1 = y.size-1
   * This constraint enforces that x and y are permutations over {0, ... , n}
   * with y(i) giving the position of number i in x. It means that x(y(i)) = i
   * Note that this constraint could be enforced with element constraints but it is less efficient
   * Weak and Strong consistency can be used acting on the filtering of alldifferent constraints
   * @param x
   * @param y of same size as x
   */
  def permutation(x: IndexedSeq[CPVarInt], y: IndexedSeq[CPVarInt]): Constraint = {
    new Permutation(x, y)
  }

  def sortedness(x: IndexedSeq[CPVarInt], s: IndexedSeq[CPVarInt], p: IndexedSeq[CPVarInt],strictly: Boolean = false): LinkedList[Constraint] = {
    val cp = x(0).store
    val n = x.size
    val cons = new LinkedList[Constraint]
    for (i <- 0 until n - 1) {
      cons.add(elementVar(x, p(i),Strong) <= elementVar(x, p(i + 1),Strong))
      if (strictly) {
        cons.add(s(i) < s(i + 1))
      } else {
        cons.add(s(i) <= s(i + 1))
      }
      
    }
    val minx = x.map(_.min).min
    val maxx = x.map(_.max).max
    val mins = s.map(_.min).min
    val maxs = s.map(_.max).max
    
    for( i <- 0 until x.size) {
      cons.add(p(i) >= 0)
      cons.add(p(i) <= n)
      
      cons.add(s(i) <= maxx)
      cons.add(s(i) >= minx)
      
      cons.add(x(i) <= maxs)
      cons.add(x(i) >= mins)
    }
    for (i <- 0 until n) {
      cons.add(elementVar(x, p(i), s(i)))
    }
    cons.add(allDifferent(p))

    val minVal: Int = x.map(_.min).min
    val maxVal: Int = x.map(_.max).max

    // array of variable occ with domains {0,...,n} that will represent the number of occurrences of each value
    val occ = Array.fill(maxVal - minVal + 1)(CPVarInt(cp, 0 to n))
    cons.add(gcc(x, (minVal to maxVal).zip(occ)))

    // nbBefore(i) = #{i | x(i) < i } i.e. number of values strictly small than i for i in [minVal .. maxVal]
    val nbBefore = for (i <- minVal to maxVal) yield {
      if (i == minVal) CPVarInt(cp, 0)
      else sum(minVal to i - 1)(j => occ(j))
    }

    for (i <- 0 until n) {
      // there are less than i values smaller than s(i) 
      cons.add(elementVar(nbBefore, s(i) - minVal) <= i)
    }
    cons
  }
  // scheduling constraints

  /**
   * Unary Resource constraint
   */
  def unaryResource(activities: Array[Activity], name: String = "machine"): UnaryResource = {
    new UnaryResource(activities, name)
  }

  /**
   * Cumulative
   */
  def cumulative(activities: Array[CumulativeActivity], machine: Int, min: Int = Int.MinValue, max: Int = Int.MaxValue): Constraint = {

    val cp = activities(0).store

    if (max != Int.MaxValue) {
      if (min != Int.MinValue) {
        return new BoundedSweepCumulative(cp, activities, min, max, machine)
      } else {
        return new MaxSweepCumulative(cp, activities, max, machine)
      }
    } else if (min != Int.MinValue) {
      return new MinSweepCumulative(cp, activities, min, machine)
    }

    throw new IllegalArgumentException("Bounds are not specified")
  }
  
  /**
   * Constraint x and y to be disjoint (no common values)
   * @param x:
   * @param y:
   */
  def disjoint(x: CPVarSet, y: CPVarSet): Constraint = new Disjoint(x,y)
}
