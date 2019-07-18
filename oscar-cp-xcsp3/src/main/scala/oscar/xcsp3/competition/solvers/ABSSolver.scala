package oscar.xcsp3.competition.solvers

import oscar.algo.Inconsistency
import oscar.cp.core.NoSolutionException
import oscar.cp.heuristics.{ActivityBasedSearch, BoundImpactValueSelector, InitHeuristics, InitHeuristics2, NewBIVS2, ObjectiveBasedSelector, ObjectiveLandscape, SolBasedPhaseSaving}
import oscar.cp.nogoods.database.NogoodDB
import oscar.cp.nogoods.searches.{BinaryRandomizedNogoodBranching, ConflictOrderingSearch, HeuristicNogoodBranching, NogoodSearch}
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.{CPSolver, _}
import oscar.modeling.models.cp.CPModel
import oscar.modeling.models.operators.CPInstantiate
import oscar.modeling.models.{ModelDeclaration, UninstantiatedModel}
import oscar.xcsp3.XCSP3Parser2
import oscar.xcsp3.competition.{CompetitionApp, CompetitionConf}
import oscar.cp.heuristics.HelperFunctions._

import scala.collection.mutable
import scala.util.Random



object ABSSolver extends CompetitionApp with App {


  override def runSolver(conf: CompetitionConf): Unit = {
    val startTime = System.nanoTime()

    val md = new ModelDeclaration

    //Parsing the instance
    printComment("Parsing instance " + args(0))
    val parsingResult = try {
      val (decisionVars, auxiliaryVars, solutionGenerator) = XCSP3Parser2.parse2(md, conf.benchname())

      val model: CPModel = CPInstantiate(md.getCurrentModel.asInstanceOf[UninstantiatedModel])
      md.setCurrentModel(model)

      val cpDecisionVars: Array[CPIntVar] = decisionVars.map(model.getRepresentative(_).realCPVar)
      val cpAuxiliaryVars: Array[CPIntVar] = auxiliaryVars.map(model.getRepresentative(_).realCPVar)
      val solver: CPSolver = model.cpSolver

      Some(cpDecisionVars, cpAuxiliaryVars, solver, solutionGenerator)
    } catch {
      case _: NotImplementedError =>
        status = "UNSUPPORTED"
        printStatus()
        None

      case _: NoSolutionException =>
        status = "UNSATISFIABLE"
        printStatus()
        None

      case _: Inconsistency =>
        status = "UNSATISFIABLE"
        printStatus()
        None
    }

    if (parsingResult.isDefined) {

      val (decVars, auxVars, solver, solutionGenerator) = parsingResult.get
      val vars = decVars ++ auxVars
      solver.silent = true
      val timeout = ((conf.timelimit() - 5).toLong * 1000000000L) - (System.nanoTime() - tstart)
      val endTime = System.nanoTime() + timeout
      val maximizeObjective: Option[Boolean] = if (solver.objective.objs.nonEmpty) Some(solver.objective.objs.head.isMax) else None
      val sols = mutable.ListBuffer[(CPIntSol, String)]()

      // parameters for obs
      val isMin = solver.objective.objs.head.isMin
      val alpha = if (isMin) -1 else 0
      val beta = if (isMin) 0 else -1
      val gamma = 0.5
      val decay = 0.99
      var optimumFound = false
      var lastSolTime = 0L
      var nTimeOuts = 0
      var limit:Long = 0
      val rand = new Random(42)

      /** ******************************************************************//**
        * STEP 1 : Extract decision variables based on constraint degree
        * **********************************************************************/

      var bestScore = 0.0
      var minNumber = Double.MaxValue
      var bestId = decVars(0).name.split("\\[")(0)
      var degree = 0.0
      var currentId = ""
      var domSize = 0.0
      for (v <- decVars) {
        val subId = v.name.split("\\[")(0)
        if (subId != currentId) {
          if (domSize > 1.0 && degree / domSize > bestScore) {
            bestScore = degree / domSize
            minNumber = domSize
            bestId = currentId
          }
          else if (domSize > 1.0 && degree / domSize == bestScore) {
            if (domSize < minNumber) {
              minNumber = domSize
              bestId = currentId
            }
          }
          degree = 0
          domSize = 0
        }
        currentId = subId
        degree += v.constraintDegree
        domSize += 1
      }

      printComment("Decision variables : " + bestId)

      // split variables into decision and auxiliary variables
      val decisionVars = vars.filter(v => v.name.contains(bestId))
      val auxiliaryVars = vars.filterNot(v => v.name.contains(bestId))
      val allVars = decisionVars ++ auxiliaryVars

      /** ******************************************************************//**
        * STEP 2: Initialize heuristics / searches
        * **********************************************************************/


      // create an objective landscape on decision variables
      var objLandscape = new ObjectiveLandscape(solver, decisionVars)

      // the value heuristic used for probing is the one that is later used in the search
      val decBIVS = new BoundImpactValueSelector(solver, decisionVars)
      // initialize ABS and OBS features by probing
      val init = new InitHeuristics2(solver, decisionVars, allVars, decBIVS.selectValue, maxTime = 10000000000L, significance = 0.2)

      // set OBS and ABS values for decision variables
      val decABS = new ActivityBasedSearch(decisionVars, decay = decay)
      decABS.setActivity(init.getDecisionArrays._1)
      // set ABS and OBS values for auxiliary variables -> nothing happens if they are empty
      val auxABS = new ActivityBasedSearch(auxiliaryVars, decay = decay)
      auxABS.setActivity(init.getAuxiliaryArrays._1)
      val auxBIVS = new BoundImpactValueSelector(solver, auxiliaryVars)

      // Initialize Solution Based Phase saving -> needs to be called each time a solution is found
      val decSBPS = new SolBasedPhaseSaving(decisionVars, isMin, decBIVS.selectValue, landscape = objLandscape.getLandscapeScore)
      val auxSBPS = new SolBasedPhaseSaving(auxiliaryVars, isMin, auxBIVS.selectValue, landscape = objLandscape.getLandscapeScore)


      // Initialize noGood Searches
      val noGoodDB = NogoodDB()
      val noGoodSearch = new NogoodSearch(solver, noGoodDB)


      noGoodSearch.onSolution {
        nTimeOuts = 0
        val time = System.nanoTime() - startTime
        lastSolTime = time
        val sol = new CPIntSol(allVars.map(_.value), if (maximizeObjective.isDefined) solver.objective.objs.head.best else 0, time)
        decSBPS.updateSolution()
        auxSBPS.updateSolution()
        val instantiation = solutionGenerator()
        optimumFound = if (maximizeObjective.isDefined) solver.objective.isOptimum() else true //In case of CSP, no point of searching another solution
        if (sols.isEmpty || (maximizeObjective.isDefined && ((maximizeObjective.get && sol.objective > sols.last._1.objective) || (!maximizeObjective.get && sol.objective < sols.last._1.objective)))) {
          updateSol(instantiation, sol.objective, maximizeObjective.isDefined)
          sols += ((sol, instantiation))
        }
      }


      /** ******************************************************************//**
        * STEP 2: Search for a first solution
        * **********************************************************************/


      var searchStrat = if (auxiliaryVars.isEmpty) {
        ConflictOrderingSearch(new BinaryRandomizedNogoodBranching(decisionVars, decABS.getActivityOverDom, decBIVS.selectValue))(solver)
      }
      else {
        val h1 = new BinaryRandomizedNogoodBranching(decisionVars, decABS.getActivityOverDom, decBIVS.selectValue)
        val h2 = new BinaryRandomizedNogoodBranching(auxiliaryVars, auxABS.getActivityOverDom, auxBIVS.selectValue)
        ConflictOrderingSearch(h1)(solver) ++ ConflictOrderingSearch(h2)(solver)
      }


      var current: Long = 0L
      current = System.nanoTime()

      var stopCondition = (_: NogoodSearch) => {
        val now = System.nanoTime()
        var stop = false
        stop |= noGoodSearch.nBacktracks >= allVars.length * 2
        stop |= now >= endTime
        stop |= sols.nonEmpty
        stop |= noGoodSearch.isCompleted
        stop
      }


      while (sols.isEmpty && current < endTime && !noGoodSearch.isCompleted) {
        noGoodSearch.start(searchStrat, stopCondition)
        try {
          solver.add(noGoodDB.allNogoods().map(ng => ng.toConstraint))
        }
        catch {
          case _: NoSolutionException => {
            if (sols.isEmpty) {
              status = "UNSATISFIABLE"
              printStatus()
              return
            }
          }
        }
        noGoodDB.clear()
        current = System.nanoTime()
      }

      /** ******************************************************************//**
        * STEP 3: Improve the solution
        * **********************************************************************/

      // no time to improve the solution :(
      current = System.nanoTime()
      if (current >= endTime || noGoodSearch.isCompleted) {
        if (noGoodSearch.isCompleted && sols.isEmpty) status = "UNSATISFIABLE"
        printStatus()
        return
      }


      searchStrat = if (auxiliaryVars.isEmpty) {
        new BinaryRandomizedNogoodBranching(decisionVars, decABS.getActivityOverDom, decSBPS.selectValue)
      }
      else {
        val h1 = new BinaryRandomizedNogoodBranching(decisionVars, decABS.getActivityOverDom, decSBPS.selectValue)
        val h2 = new BinaryRandomizedNogoodBranching(auxiliaryVars, auxABS.getActivityOverDom, auxSBPS.selectValue)
        h1 ++ h2
      }

      stopCondition = (_: NogoodSearch) => {
        val now = System.nanoTime()
        var stop = false
        stop |= now >= limit
        stop |= now >= endTime
        stop |= noGoodSearch.isCompleted
        stop
      }


      val duration = luby(minTime=2, maxTime = 8)(_)
      current = System.nanoTime()
      var searchTime = duration(nTimeOuts)
      limit = current + searchTime

      while (current < endTime && !noGoodSearch.isCompleted) {

        try {
          decSBPS.addConstraints()
        }
        catch {
          case _: Inconsistency => {
            status = "OPTIMUM FOUND"
            printStatus()
            return
          }
        }
        noGoodSearch.start(searchStrat, stopCondition)
        try {
          solver.add(noGoodDB.allNogoods().map(ng => ng.toConstraint))
        }
        catch {
          case _: NoSolutionException => {
            status = "OPTIMUM FOUND"
            printStatus()
            return
          }
        }
        noGoodDB.clear()
        current = System.nanoTime()
        searchTime = duration(nTimeOuts)
        limit = current + searchTime
        nTimeOuts += 1
      }

      try {
        decSBPS.addConstraints()
      }
      catch {
        case _: Inconsistency => {
          status = "OPTIMUM FOUND"
          printStatus()
          return
        }
      }

      // no time to improve the solution :(
      current = System.nanoTime()
      if(current >= endTime || noGoodSearch.isCompleted) {
        if(noGoodSearch.isCompleted) status = "OPTIMUM FOUND"
        printStatus()
        return
      }

      stopCondition = (_: NogoodSearch) => {
        val now = System.nanoTime()
        var stop = false
        stop |= now >= endTime
        stop |= noGoodSearch.isCompleted
        stop
      }

      println("trying to prove optimality")
      searchStrat = if(auxiliaryVars.isEmpty) {
        ConflictOrderingSearch(new HeuristicNogoodBranching(decisionVars, decABS.getActivity, i => decisionVars(i).randomValue(rand)))(solver)
      }
      else {
        val h1 = new HeuristicNogoodBranching(decisionVars, decABS.getActivity, i => decisionVars(i).randomValue(rand))

        val h2 = new HeuristicNogoodBranching(auxiliaryVars, auxABS.getActivity, i => auxiliaryVars(i).randomValue(rand))
        ConflictOrderingSearch(h1)(solver) ++ ConflictOrderingSearch(h2)(solver)
      }
      noGoodSearch.start(searchStrat, stopCondition)


      if (noGoodSearch.isCompleted) {
        status = "OPTIMUM FOUND"
      }
      else {
        printStatus()
      }
    }
  }
}
