/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.cp.modeling


import scampi.cp.constraints._
import scampi.cp.core._
import scampi.cp.scheduling._

import scala.collection._
trait Constraints {

  /**
   * Bin-Packing Constraint linking the placement variables of sized items into bins with
   * the total size of the bins
   * @param x with x(i) is the bin where the item i is placed
   * @param w with w(i) is the size of item i
   * @param l with l(j) is the load of bin j
   * @return a binpacking constraint linking the variables in argument such that l[i] == sum,,j,, w[j]*x[i] for all bins i
   */
  def binpacking(x: IndexedSeq[CPVarInt], w: IndexedSeq[Int], l: IndexedSeq[CPVarInt]): Constraint = {
    return new BinPacking(x.toArray, w.toArray, l.toArray)
  }
  
  def binpackingflow(x: Array[CPVarInt], w: Array[Int], l: Array[CPVarInt]): Constraint = {
    return new BinPackingFlow(x, w, l)
  }
  
  /**
   * Binary-Knapsack Constraint computing the total weight of items placed into a knapsack
   * @param x with x(i) == 1 if the item is selected, 0 otherwise
   * @param w with w(i) is the weight of item i
   * @param l the load of the knapsack
   * @return a binary-knapsack constraint linking the variables in argument such that l == sum,,j,, w[j]*x[i]
   */
  def binaryknapsack(x: IndexedSeq[CPVarBool], w: IndexedSeq[Int], l: CPVarInt): Constraint = {
    return new BinaryKnapsack(x.toArray, w.toArray, l)
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
  def binaryknapsack(x: IndexedSeq[CPVarBool], p: IndexedSeq[Int], w: IndexedSeq[Int], P: CPVarInt, W: CPVarInt): Knapsack = {
    return new Knapsack(x.toArray,p.toArray,w.toArray,P,W)
  }
  


  /**
   * AllDifferent Constraint (Available Filtering: Weak, Strong)
   * @param vars an non empty array of variables
   * @return a constraint ensure that no value occurs more than once in vars
   */
  def alldifferent(vars: CPVarInt*): Constraint = {
    return new AllDifferent(vars: _*)
  }
  
  def alldifferent(vars: Iterable[CPVarInt]): Constraint = {
    return alldifferent(vars.toArray: _*)
  }

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
  def lexleq(x: Array[CPVarInt], y: Array[CPVarInt]): Constraint = {
    new LexLeq(x, y)
  }

  /**
   * Element Constraint, indexing an array of integers by a variable
   * @param tab an non empty array n integers
   * @param x an index variable with domain defined on (0..n-1)
   * @return a variable z linked to tab and x by the relation tab(x) == z
   */
  def element(tab: Array[Int], x: CPVarInt): CPVarInt = {
    val minval = tab.min
    val maxval = tab.max
    val z = new CPVarInt(x.getStore, minval, maxval)
    x.getStore.post(new ElementCst(tab, x, z))
    z
  }

  /**
   * Element Constraint, indexing an array of integers by a variable
   * @param tab an non empty array n integers
   * @param x an index variable with domain defined on (0..n-1)
   * @param z an integer variable
   * @return a constraints such that tab , x and z are linked by the relation tab(x) == z
   */
  def element(tab: Array[Int], x: CPVarInt, z: CPVarInt): Constraint = {
    new ElementCst(tab, x, z)
  }

  /**
   * Element Constraint, indexing an array of integers by a variable
   * @param tab an non empty array n integers
   * @param x an index variable with domain defined on (0..n-1)
   * @param z an integer
   * @return a constraints such that tab, x and z are linked by the relation tab(x) == z
   */
  def element(tab: Array[Int], x: CPVarInt, z: Int): Constraint = {
    new ElementCst(tab, x, new CPVarInt(x.getStore, z, z))
  }

  /**
   * Element Constraint, indexing an array of variables by a variable
   * @param tab an non empty array n variables
   * @param x an index variable with domain defined on (0..n-1)
   * @return an integer variable z such that tab, x and z are linked by the relation tab(x) == z
   */
  def element(tab: Array[CPVarInt], x: CPVarInt): CPVarInt = {
    val minval = (for(x <- tab) yield x.getMin) min
    val maxval = (for(x <- tab) yield x.getMax) max
    val z = new CPVarInt(x.getStore, minval, maxval)
    x.getStore.add(new ElementVar(tab, x, z))
    z
  }

  /**
   * Element Constraint, indexing an array of variables by a variable
   * @param tab an non empty array n variables
   * @param x an index variable with domain defined on (0..n-1)
   * @param z an integer variable
   * @return a constraints such that tab , x and z are linked by the relation tab(x) == z
   */
  def element(tab: Array[CPVarInt], x: CPVarInt, z: CPVarInt): Constraint = {
    new ElementVar(tab, x, z)
  }

  /**
   * Element Constraint, indexing an array of variables by a variable
   * @param tab an non empty array n variables
   * @param x an index variable with domain defined on (0..n-1)
   * @param z an integer
   * @return a constraints such that tab, x and z are linked by the relation tab(x) == z
   */
  def element(tab: Array[CPVarInt], x: CPVarInt, z: Int): Constraint = {
    new ElementVar(tab, x, new CPVarInt(x.getStore, z, z))
  }

  /**
   * Element 2D Constraint, indexing an integer matrix by two index variables
   * @param matrix rectangle matrix of sizes n x m
   * @param i the first index variable (line index) with domain defined on (0..n-1)
   * @param j the second index variable (column index) with domain defined on (0..m-1)
   * @return a variable z linked to the arguments with the relation matrix(i)(j) == z
   */
  def element(matrix: Array[Array[Int]], i: CPVarInt, j: CPVarInt) = {
    ElementCst2D.get(matrix, i, j)
  }


  /**
   * Sum Constraint
   * @param vars a non empty array of n variables
   * @param s a variable representing the sum of vars
   * @return a constraint enforcing vars(0)+vars(1)+...+vars(n) = s
   */
  def sum(vars: Array[CPVarInt], s: CPVarInt): Constraint = {
    new Sum(vars, s)
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
    val s = new CPVarInt(x(0).getStore, minVal, maxVal)
    x(0).getStore.post(sum(x, s))
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
   * Sum Consraint
   * @param indexes1 a first iterable
   * @param indexes2 a second iterable
   * @param f a function mapping A,B to a variable with A from indexes1 and B from indexes2
   * @return a variable that is the sum of f(A,B) over each (A, B) in (indexes x indexes2)
   */
  def sum[A, B](indexes1: Iterable[A], indexes2: Iterable[B])(f: (A, B) => CPVarInt): CPVarInt = {
    sum(for (i <- indexes1; j <- indexes2) yield f(i, j))
  }

  /**
   * Or (logical) Constraint
   * @param vars a non empty array of n variables
   * @return a variable that will be true if at least one variable of vars is true
   */
  def or(vars: Array[CPVarBool]): CPVarBool = {
    val z = new CPVarBool(vars(0).getStore)
    vars(0).getStore.post(new Or(vars, z))
    return (z)
  }
  
  
  def table(x: Array[CPVarInt], tuples: Array[Array[Int]]): Constraint = {
    new TableSTR2(x,tuples)
  }


  /**
   * Table Constraints for couples (constraint given in extension by enumerating valid assignments)
   * @param x1 first variable
   * @param x2 second variable
   * @param tuples a collection of coulples
   * @return a constraint enforcing that (x1,x2) is one of the couples given in tuples
   */
  def table(x1: CPVarInt, x2: CPVarInt, tuples: Iterable[(Int,Int)]): Constraint = {
    table(Array(x1,x2),tuples.map(t => Array(t._1,t._2)).toArray)
    /*
    println("new table")
    val tableCons = new Table(x1,x2)
    for (t <- tuples) {
      tableCons.addTupple(t._1, t._2)
    }
    tableCons
  	*/
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
    table(Array(x1,x2,x3),tuples.map(t => Array(t._1,t._2,t._3)).toArray)
    /*
    val tableCons = new Table(x1,x2,x3)
    for (t <- tuples) {
      tableCons.addTupple(t._1, t._2, t._3)
    }
    tableCons
  	*/
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
    table(Array(x1,x2,x3,x4),tuples.map(t => Array(t._1,t._2,t._3,t._4)).toArray)
    /*
    val tableCons = new Table(x1, x2, x3, x4)
    for (t <- tuples) {
      tableCons.addTupple(t._1, t._2, t._3, t._4)
    }
    tableCons
    */
  }
  
  def modulo(x: CPVarInt, v: Int, y: CPVarInt): Constraint = {
    return new Modulo(x,v,y)
  }  

  def modulo(x: CPVarInt, v: Int, y: Int): Constraint = {
    return new Modulo(x,v,new CPVarInt(x.getStore(),y))
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

  
  def gcc(x: Array[CPVarInt], values: Range, min: Array[Int], max: Array[Int]): Constraint = {
    return new GCC(x, values.min, min, max)
  }
  
  def softgcc(x: Array[CPVarInt], values: Range, min: Array[Int], max: Array[Int], totviol: CPVarInt): Constraint = {
    return new SoftGCC(x, values.min, min, max, totviol)
  } 
  
  
  /**
   * Global Cardinality Constraint with variable counters
   * @param x a non empty array of variables
   * @param valueOccurence is an array of pairs (o,v)
   *        where o is variable representing the number of occurrences of value v
   * @return a constraint such that for each (o,v) in valueOccurrence, o is the number of times the value v appears in x
   */
  def gcc(x: Array[CPVarInt], valueOccurrence: Array[Tuple2[CPVarInt,Int]]): Constraint = {
    def freshCard(): CPVarInt = new CPVarInt(x(0).getStore, 0, x.length - 1)
    val sortedValOcc = valueOccurrence.sortWith((a, b) => a._2 <= b._2)
    val (x0,v0)  = sortedValOcc(0)
    var values = Array(v0)
    var cardinalities = Array(x0)
    for (i <- 1 until sortedValOcc.length) {
      val (xi_1,vi_1) = sortedValOcc(i - 1)
      val (xi,vi) = sortedValOcc(i)
      for (v <- (vi_1 + 1) until vi) {
        values = values :+ v
        cardinalities = cardinalities :+ freshCard()
      }
      values = values :+ vi
      cardinalities = cardinalities :+ xi
    }
    new GCCVar(x, values(0), cardinalities)
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
    	transitions.foreach(t => {transiFrom = transiFrom :+ t._1; transiTo = transiTo :+ t._2})
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
    val cp = x(0).getStore
    val m = new CPVarInt(cp, vars.map(_.getMin).max, vars.map(_.getMax).max)
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
    val cp = x(0).getStore
    val m = new CPVarInt(cp, vars.map(_.getMin).max, vars.map(_.getMax).max)
    cp.add(minimum(x, m))
    m
  }


  // scheduling constraints

  /**
   * Unary Resource constraint
   */
  def unaryResource(activities: Array[Activity], name: String = "machine"): UnaryResource = {
    new UnaryResource(activities, name)
  }


}