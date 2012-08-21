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

package oscar.linprog.modeling

import gurobi._
import scala.collection.mutable.ListBuffer

/**
 * @author Hrayr Kostanyan Hrayr.Kostanyan@ulb.ac.be, Pierre Schaus pschaus@gmail.com
 */
class GurobiLP extends AbstractLP{
    println("GurobiLP")

	var nbRows = 0
  	var nbCols = 0
    var sol = Array[Double]()
  	var objectiveValue = 0.0
	var status = LPStatus.NOT_SOLVED
 	var closed = false
  	var released = false
  	var Obj=0.0
  	
  	var env = new GRBEnv()
	var  model = new GRBModel(env)
			  
	def startModelBuilding(nbRows : Int, nbCols : Int) {

		this.nbRows = nbRows
		this.nbCols = nbCols		
		for (i <- 1 to nbCols ){
		   var x=model.addVar(0.0, GRB.INFINITY, 0.0, GRB.CONTINUOUS, "x"+i)
		}
		model.update()
	}
	
	def endModelBuilding() {
		 closed = true
	}
		
	def setVarName(colId : Int, name: String) { 
	  // TODO implement
	}
	def addConstraint(coef : Array[Double], col : Array[Int], rhs : Double, sign: String, name:String){
		nbRows += 1	
		
		var ntot = new GRBLinExpr()
    	for (i <- 1 to col.size )
    		ntot.addTerm(coef(i-1),model.getVar(col(i-1)))       
        sign match {
        	case "<=" =>
         		model.addConstr(ntot,GRB.LESS_EQUAL,rhs,name)
         	case ">=" =>
         		model.addConstr(ntot,GRB.GREATER_EQUAL,rhs,name)
         	case "==" =>
         		model.addConstr(ntot,GRB.EQUAL,rhs,name)
        }
        model.update()
	}
	def addConstraintGreaterEqual(coef : Array[Double], col : Array[Int], rhs : Double, name:String) {
		addConstraint(coef,col,rhs,">=", name)
	}
    
	def addConstraintLessEqual(coef : Array[Double], col : Array[Int], rhs : Double, name:String) {	
		addConstraint(coef,col,rhs,"<=", name)
	}
    def addConstraintEqual(coef : Array[Double], col : Array[Int], rhs : Double, name:String) {      
    	addConstraint(coef,col,rhs,"==", name)      
    }
    
    def addObjective(coef : Array[Double], col : Array[Int], minMode : Boolean = true) {    	
    	var ntot = new GRBLinExpr()
    	
    	for (i <- 1 to col.size ){
  		ntot.addTerm(coef(i-1),model.getVar(col(i-1)))
    	}
  		model.setObjective(ntot)
  		model.update()
  		model.set(GRB.IntAttr.ModelSense, if (minMode) 1 else -1)  	
    }
    
  	def addColumn(obj : Double, row : Array[Int], coef : Array[Double]) {
  		if (!closed) {
  			println("cannot add a column in a non closed solver")
  		} else{
  			nbCols+=1  		
  			val col = new GRBColumn()
  			for (i <- 1 to row.size ){
  				col.addTerm(coef(i-1),model.getConstr(row(i-1)) )
  			}
  			val v=model.addVar(0.0, GRB.INFINITY, obj, GRB.CONTINUOUS,col, "x"+nbCols)
  			model.update()
  		}
  	}
  	
  	def getLowerBound(colId : Int) : Double = {
  		model.getVar(colId).get(GRB.DoubleAttr.LB)
  	}
  	
  	def getUpperBound(colId : Int) : Double = {
  		model.getVar(colId).get(GRB.DoubleAttr.UB)
  		
  	}
  	
  	def updateLowerBound(colId : Int, lb : Double) {
  		model.getVar(colId).set(GRB.DoubleAttr.LB,lb)
  		
  	}
  	
    def updateUpperBound(colId : Int, ub : Double) {	
    	model.getVar(colId).set(GRB.DoubleAttr.UB,ub)
    
    	
    }
    
    def solveModel(): LPStatus.Value = {
    		model.write("gurobi.lp")
    		model.getEnv().set(GRB.IntParam.Presolve, 0)
    		model.optimize()

    		var optimstatus = model.get(GRB.IntAttr.Status)

    		if (optimstatus == GRB.INF_OR_UNBD) {
    			model.getEnv().set(GRB.IntParam.Presolve, 0)
    			model.optimize()
    			optimstatus = model.get(GRB.IntAttr.Status)
    			LPStatus.UNBOUNDED
    		}
    		else if (optimstatus == GRB.OPTIMAL) {
    		   sol= Array.tabulate(nbCols)(col => model.getVar(col).get(GRB.DoubleAttr.X))
    			Obj=model.get(GRB.DoubleAttr.ObjVal)
    			LPStatus.OPTIMAL
    		} else if (optimstatus == GRB.INFEASIBLE) {
    			println("Model is infeasible")

    			// compute and write out IIS
    			model.computeIIS()
    			LPStatus.INFEASIBLE
    		} else if (optimstatus == GRB.UNBOUNDED) {
    			println("Model is unbounded")
    			LPStatus.UNBOUNDED
    		} else {
    			sol= Array.tabulate(nbCols)(col => model.getVar(col).get(GRB.DoubleAttr.X))
    			println("Optimization was stopped with status = " + optimstatus)
    			LPStatus.SUBOPTIMAL
    		}
    } 

    
    def getValue(colId : Int) : Double = {
    		sol(colId)

    }
    
  	def getObjectiveValue() : Double = {
  		Obj
  	}
  	
  	def setInteger(colId : Int) {
  		model.getVar(colId).set(GRB.CharAttr.VType,'I')

  	}
  	
  	def setFloat(colId : Int) {

  	  
  	}
  	
  	def setBounds(colId : Int, low : Double, up : Double) {
  		var ntot = new GRBLinExpr()
  		ntot.addTerm(1,model.getVar(colId))
  		model.addRange(ntot,low,up,"")
  		model.update()
  	}
  	
  	def setUnboundUpperBound(colId : Int) {
  	
  	}
  	
  	def setUnboundLowerBound(colId : Int) {
  		model.getVar(colId).set(GRB.DoubleAttr.LB,-GRB.INFINITY)
  	}
  	
  	def getReducedCost(colId : Int) : Double = {
  		  model.getVar(colId).get(GRB.DoubleAttr.RC)
  	}
  	
  	def getDual(rowId : Int) : Double = {
  		model.getConstr(rowId).get(GRB.DoubleAttr.Pi)
  	}
  	
  	def deleteConstraint(rowId : Int) {
  		model.remove(model.getConstr(rowId))
  	}

  	def addVariable() {
  		nbCols+=1
  		var x=model.addVar(0.0, GRB.INFINITY, 0.0, GRB.CONTINUOUS, "x"+nbCols)
  		model.update()
  	}
  	
  	def deleteVariable(colId : Int) {
  		model.remove(model.getVar(colId))
  	}
  	def release(){
  		model.reset
  	}
  	
  	def exportModel(fileName: String) {
  		model.write(fileName)
  	}
}