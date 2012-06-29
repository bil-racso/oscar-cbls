/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

/*
 * Copyright CETIC 2012 www.cetic.be
 *
 * This file is part of Asteroid.
 *
 * Asteroid is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Asteroid is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asteroid.
 * If not, see http://www.gnu.org/licenses/lgpl-2.1-standalone.html
 *
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 */

package oscar.cbls.invariants.lib.logic
/**This package proposes a set of logic invariants, which are used to define the structure of the problem*/

import collection.immutable.SortedSet;
import collection.immutable.SortedMap;
import oscar.cbls.invariants.core.computation.IntVar._

import oscar.cbls.invariants.core.computation._;

/**maintains a cluster of the indexes of array:  cluster(j) = {i in index of values | values[i] == j}
 * This is considered as a sparse cluster because Cluster is a map and must not cover all possibles values of the values in the array ''values''
 * */
case class SparseCluster(var values:Array[IntVar], Clusters:SortedMap[Int,IntSetVar]) extends Invariant {

  for (v <- values.indices) registerStaticAndDynamicDependency(values(v),v)

  finishInitialization()

  for(c <- Clusters.values){c.setDefiningInvariant(this); c.setValue(SortedSet.empty)}

  for(v <- values.indices){
    val x:IntSetVar = Clusters.getOrElse(values(v).getValue(),null)
    if(x != null) x.insertValue(v)
  }

  @inline
  override def notifyIntChanged(v:IntVar,index:Int, OldVal:Int,NewVal:Int){
    val x:IntSetVar = Clusters.getOrElse(OldVal,null)
    if(x != null) x.deleteValue(index)
    val y:IntSetVar = Clusters.getOrElse(NewVal,null)
    if(y != null) y.insertValue(index)
  }

  override def checkInternals(){
    for(v <- values.indices){
      if (Clusters.isDefinedAt(values(v))){assert(Clusters(values(v)).contains(v))}
    }
    for(value <- Clusters.keys){
      for (indices <- Clusters(value)){
        assert(values(indices).getValue() == value)
      }
    }
  }
}

/**Maintains a cluster of the indexes of array: cluster(j) = {i in index of values | values[i] == j}
 * This is considered as a dense cluster because Cluster is an array and must cover all the possibles values of the values in the array ''values''
 * */
case class DenseCluster(var values:Array[IntVar], clusters:Array[IntSetVar]) extends Invariant {

  //We register the static and dynamic dependencies.
  //Dynamic dependencies are the ones considered for the notifications.
  //Static dependencies are the ones considered for ordering the propagations
  for (v <- values.indices) registerStaticAndDynamicDependency(values(v),v)

  //This must be called once all static dependencies are registered
  //It must be called before the output dependencies are notified
  finishInitialization()

  //We then define the variable that we control
  //By theway, an initial value is set to each of them (SortedSet.empty)
  for(c <- clusters){
    c.setDefiningInvariant(this) //A variable can only have a single controlling invariant
    c.setValue(SortedSet.empty)
  }

  //We then complete the initialization the output variables to the value they should have
  for(v <- values.indices){
    clusters(values(v).getValue()).insertValue(v)
  }

  //This method is called by each IntVar that is registered to the dynamic dependency graph.
  //We update the output variabls incrementally based on this update.
  @inline
  override def notifyIntChanged(v:IntVar,index:Int,OldVal:Int,NewVal:Int){
    assert(values(index) == v)
    clusters(OldVal).deleteValue(index)
    clusters(NewVal).insertValue(index)
  }

  //This method is optionnal, it is called by the model when its debug mode is activated (see the contructor of model)
  //In this method, we check that the outputs are correct, based on non-incremental code
  override def checkInternals(){
    for(v <- values.indices){
      assert(clusters(values(v)).contains(v))
    }
    for(value <- clusters.indices){
      for (indices <- clusters(value)){
        assert(values(indices).getValue() == value)
      }
    }
  }
}

/**This is a helper object for the [[invariants.lib.logic.DenseCluster]] and [[invariants.lib.logic.SparseCluster]] invariants.*/
object Cluster{

  def MakeSparse(values:Array[IntVar], clusters: Iterable[Int]):SparseCluster = {
    val m:Model = InvariantHelper.FindModel(values)
    val Clusters:SortedMap[Int,IntSetVar] = clusters.foldLeft(SortedMap.empty[Int, IntSetVar])((acc,c) => acc + ((c,new IntSetVar(m,values.indices.start,values.indices.end,"cluster_"+c))))
    SparseCluster(values,Clusters)
  }

  def MakeDense(values:Array[IntVar]):DenseCluster = {
    val themax = values.foldLeft(Int.MinValue)((acc,intvar) => if (acc < intvar.MaxVal) intvar.MaxVal else acc)
    val themin = values.foldLeft(Int.MaxValue)((acc,intvar) => if (acc > intvar.MinVal) intvar.MinVal else acc)
    assert(themin == 0, "dense clusters must start at zero")
    val m:Model = InvariantHelper.FindModel(values)
    val Clusters:Array[IntSetVar] = (for(c <- 0 to themax) yield new IntSetVar(m,values.indices.start,values.indices.end,"cluster_"+c)).toArray
    DenseCluster(values,Clusters)
  }
  def MakeDenseAssumingMinMax(values:Array[IntVar],themin:Int,themax:Int):DenseCluster = {
    assert(themin == 0, "dense clusters must start at zero")
    val m:Model = InvariantHelper.FindModel(values)
    val Clusters:Array[IntSetVar] = (for(c <- 0 to themax) yield new IntSetVar(m,values.indices.start,values.indices.end,"cluster_"+c)).toArray
    DenseCluster(values,Clusters)
  }
}
