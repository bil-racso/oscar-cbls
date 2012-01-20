/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 * 
 * Contributors:
 *     www.n-side.com
 ******************************************************************************/
package scampi.dfo.modeling


import scala.collection._
import scampi.dfo.algo._
import scampi.dfo.utils._
import scampi.algebra._

/**
 * Trait used for the modeling of single objective minimization using DFO
 * @author: pierre.schaus@n-side.com
 */
trait DFOModel extends Algebra {
  
  object DFOAlgo extends Enumeration {
    val NelderMead = Value("NelderMead")
    val DDS = Value("DDS")
    val MDS = Value("MDS")
  }
  
  
  val rand = new scala.util.Random(12)
  

  case class DFOVar(val solver: DFOSolver, varName:String, val lb:Double = 0.0, val ub:Double = Double.PositiveInfinity) extends Var {
    val index = solver.register(this)
    override def value = solver.getValue(index)
    def name = varName
    def randVal = rand.nextDouble()*(ub-lb)+lb
  }
  


  
  class DFOSolver {
         
    // map from the index of variables to their implementation
    private lazy val vars = mutable.HashMap.empty[Int,DFOVar]
    private lazy val solution = mutable.HashMap.empty[Int,Double]
    

    /**
     * Single Objective DFO Algorithm used for the optimization
     */
    var algo : DFOAlgo.Value = DFOAlgo.NelderMead
    /**
     * Tolerance used to decide completion
     */
    var tolerance = Math.pow(10,-2) // tolerance
    /**
     * Maximum number of evaluation
     */
    var maxEval = 1000 // max number of evaluation
    /**
     * Maximum time of computation
     */
    var maxTime = 10 // max time(s)
 
    
    def register(vari: DFOVar): Int = {
      vars(vars.size) = vari
      vars.size-1
    }
    
    def getValue(varIndex: Int): Option[Double] = solution.get(varIndex)
    
    var onSolutionCallbacks: List[Function0[Unit]] = List()  
    
    def onSolution(block: Function0[Unit] ) : DFOSolver = {
		onSolutionCallbacks =  block :: onSolutionCallbacks
		this
	}
    
    def minimize(objective: Expression) : Expression = {
           
    	val domain = Array.tabulate(vars.size)(i => Interval(vars(i).lb,vars(i).ub))
    
    
    	def function(coord: Array[Double]): Array[Double] = {
    		val env : Map[Var,Double] = vars.map{ case(i,x) => (x -> coord(i))}
    		Array(objective.eval(env))
    	}
    	
    	def singleObjCompare(a1: Array[Double], a2:Array[Double]): Boolean = {
    		a1(0) < a2(0)
    	}
    	
    	val start = Array.tabulate(vars.size)(i => vars(i).randVal)

    	val algoImplem =  algo match {
    	  case DFOAlgo.NelderMead => NelderMead(function, 1, singleObjCompare, start, domain)
    	  case DFOAlgo.MDS => MDS(function, 1, singleObjCompare, start, domain)
    	  case DFOAlgo.DDS => {
    	    val bases = Array.tabulate(vars.size)(i => Array.tabulate(vars.size)(j => if(i==j) 1.0 else 0.0))
    	    bases.foreach(b => println(b.mkString(",")))
    	    DDS(function, 1, singleObjCompare, start, domain,bases)
    	  }
    	}
    	
    	def recordSolution(sol: (Array[Double],Array[Double])) {
    	  (0 until vars.size) foreach {i =>  solution(i) = sol._1(i)}
    	}
    	
    	onSolution { // update the sol when a new one is found
    	  () => recordSolution(algoImplem.currentBest)
    	}
    	
    	algoImplem.onImprovement = () => {
    	  onSolutionCallbacks.foreach(c => c())
    	}
    	val t0 = System.currentTimeMillis()
    	recordSolution(algoImplem.optimize(tolerance,maxEval,maxTime))
    	
    	println("------DFO Optimization Summary---------")
    	println("Algo "+algo)
    	println("Iter "+algoImplem.evalCount+"/"+maxEval)
    	println("Time "+(System.currentTimeMillis()-t0)/100.0+"/"+maxTime+" seconds")
    	println("---------------------------------------")
    	  /** The number of function evaluations performed */
    	var evalCount = 0

    	return objective  	  
    }
  }
  
  object DFOSolver {
	def apply(): DFOSolver = new DFOSolver()
  }
  
  
  
  
} // end of trait


