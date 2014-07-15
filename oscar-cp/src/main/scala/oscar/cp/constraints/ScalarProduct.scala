package oscar.cp.constraints

import oscar.cp.core._
import oscar.cp.modeling.sum


/**
 * Scalar product equality, inequality and disequality that tries to use BinarySum
 * @author Steven Gay steven.gay@uclouvain.be
 * 
 */

object ScalarProduct {
  // Constraints linked to scalar products of variables by constants
  // The goal here is to avoid the creation of new variables, so we use views wherever we can
  
  def zero(vars: Seq[CPIntVar], scalars: Seq[Int])(implicit cp: CPStore): Constraint = { // the scalar product must be zero
    assert(vars.size == scalars.size)
//    sum(vars.zip(scalars).map { case (x, c) => x * c }) == 0
    
    // step 1, take all indices where x_i * a_i is known in advance, and put these in a separate int
    val (constants, nonconstants) = vars.indices.partition { i => scalars(i) == 0 || vars(i).isBound }
    val constant = constants.foldLeft(0){ (acc, i) => acc + scalars(i) * vars(i).min }
    
    // step 2, separate positive scalars from negative ones to avoid using an opposite view
    // Distributing them on either side of the equality sign 
    val (positives, negatives) = nonconstants.partition(i => scalars(i) > 0)
    
    // step 3, use a binary sum if we can
    if (positives.size == 2 && negatives.size == 1) {
      new BinarySum( vars(positives(0)) *  scalars(positives(0)),
                     vars(positives(1)) *  scalars(positives(1)),
                     vars(negatives(0)) * -scalars(negatives(0)) - constant
                   )
    } else if (positives.size == 1 && negatives.size == 2) {
      new BinarySum( vars(negatives(0)) * -scalars(negatives(0)),
                     vars(negatives(1)) * -scalars(negatives(1)),
                     vars(positives(0)) *  scalars(positives(0)) + constant
                   )
    } else { // TODO: add more special cases, zero negative and 2 or 3 positives, the symmetrical cases...
      val positivesSum = if (!positives.isEmpty) sum(positives.map(i => vars(i) *  scalars(i))) else CPIntVar(0)
      val negativesSum = if (!negatives.isEmpty) sum(negatives.map(i => vars(i) * -scalars(i))) else CPIntVar(0)
      (positivesSum + constant == negativesSum)
    }
  }
  
  // the scalar product must be nonzero, code is a copypasta of zero
  def nonzero(vars: Seq[CPIntVar], scalars: Seq[Int])(implicit cp: CPStore): Constraint = {
    assert(vars.size == scalars.size)
//    sum(vars.zip(scalars).map { case (x, c) => x * c }) != 0
    
    val (constants, nonconstants) = vars.indices.partition { i => scalars(i) == 0 || vars(i).isBound }
    val constant = constants.foldLeft(0){ (acc, i) => acc + scalars(i) * vars(i).min }
    
    val (positives, negatives) = nonconstants.partition(i => scalars(i) > 0)
    
    // TODO: are there optimizations for special cases?
    val positivesSum = if (positives.isEmpty) CPIntVar(0) else sum(positives.map(i => vars(i) *  scalars(i)))
    val negativesSum = if (negatives.isEmpty) CPIntVar(0) else sum(negatives.map(i => vars(i) * -scalars(i)))
    // TODO: find out why first version fails tests and not second
//    (positivesSum + constant != negativesSum)
    (positivesSum + constant - negativesSum != 0)
  }
  
  def normalize(vars: Seq[CPIntVar], scalars: Seq[Int])(implicit cp: CPStore) = {
    assert(vars.size == scalars.size)
    val (constants, nonconstants) = vars.indices.partition { i => scalars(i) == 0 || vars(i).isBound }
    val constant = constants.foldLeft(0){ (acc, i) => acc + scalars(i) * vars(i).min }
    
    val (positives, negatives) = nonconstants.partition(i => scalars(i) > 0)
    
    val positivesSum = if (!positives.isEmpty) sum(positives.map(i => vars(i) *  scalars(i))) else CPIntVar(0)
    val negativesSum = if (!negatives.isEmpty) sum(negatives.map(i => vars(i) * -scalars(i))) else CPIntVar(0)
    
    (positivesSum, constant, negativesSum)
  }
  
  // the scalar product must be negative or zero, code is a copypasta of zero
  def leq(vars: Seq[CPIntVar], scalars: Seq[Int])(implicit cp: CPStore): Constraint = {
    val (ps, c, ns) = normalize(vars, scalars)
    
    // TODO: find out why first version is much slower than the second
    // (ps + c <= ns)
     (ps + c - ns <= 0)
  }

  def lt(vars: Seq[CPIntVar], scalars: Seq[Int])(implicit cp: CPStore): Constraint = {
    val (ps, c, ns) = normalize(vars, scalars)
    (ps + c - ns < 0)
  }

  def geq(vars: Seq[CPIntVar], scalars: Seq[Int])(implicit cp: CPStore): Constraint = {
    val (ps, c, ns) = normalize(vars, scalars)
    (ps + c - ns >= 0)
  }
  
  def gt(vars: Seq[CPIntVar], scalars: Seq[Int])(implicit cp: CPStore): Constraint = {
    val (ps, c, ns) = normalize(vars, scalars)
    (ps + c - ns > 0)
  }
}
