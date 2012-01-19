/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.search;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Stack;

import scampi.cp.core.CPVarInt;
import scampi.reversible.ReversibleSearchNode;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Search {
	
	private ReversibleSearchNode node;
	
	private LinkedList<SolutionObserver> solutionObservers;
	
	private LinkedList<RestartObserver> restartObservers;	
	
	private int totRestarts;
	private int totBkts;
	private int restartBkts;
	private int restartLimit = Integer.MAX_VALUE;
	private boolean solFound;
	
	private Branching branching;

	
	private int currentNode;
	
	private int timeout = -1;
	private long startTime = 0;

    private boolean stopRestarts = false;
    
    
    private Restart restart = null;
    

    /**
     * Creates a new search object on a reversible node using a particular branching.
     * @param node
     * @param branching
     */
	public Search(ReversibleSearchNode node, Branching branching) {
		this.node = node;
		totRestarts = 0;
		totBkts = 0;
		restartBkts = 0;
		solutionObservers = new LinkedList<SolutionObserver>();
		restartObservers = new LinkedList<RestartObserver>();
		solFound = false;
		this.branching = branching;
		currentNode = 0;
	}
	
    /**
     * Creates a new search object on a reversible node using a sequence of branchings.
     * @param node
     * @param branching
     */
	public Search(ReversibleSearchNode node, Branching ...branchings) {
		BranchingCombinator combi = new BranchingCombinator();
		for (Branching b: branchings) {
			combi.addBranching(b);
		}
		this.node = node;
		totRestarts = 0;
		totBkts = 0;
		restartBkts = 0;
		solutionObservers = new LinkedList<SolutionObserver>();
		restartObservers = new LinkedList<RestartObserver>();
		solFound = false;
		this.branching = combi;
		currentNode = 0;
	}
	
	
	public Search(ReversibleSearchNode node, Branching branching,String treexml) {
		this(node,branching);
	}

    /**
     * Set a maximum number of seconds allowed for the search
     * @param nbseconds
     */
    public void setTimeLimit(int nbseconds) {
		if (nbseconds <= 0) {
			throw new RuntimeException("Search: timeout must be positive");
		}
		this.timeout = nbseconds;
	}
	
	private boolean isTimeout() {
		if (timeout < 0) {
			return false;
		}
		long t = System.currentTimeMillis();
		return (t-startTime)/1000 >= timeout;
	}

    /**
     * Set the branching to use for the search (can be used to change it dynamically)
     * @param branching
     */
	public void setBranching(Branching branching) {
		this.branching = branching;
	}

    /**
     * add an observer to be notified on each feasible solution found during a solveOne or solveAll.
     * @param obs
     */
	public void addSolutionObserver(SolutionObserver obs) {
		solutionObservers.add(obs);
	}
	
	private void notifySolution(){
		solFound = true;
		for(SolutionObserver obs: solutionObservers) {
			obs.solutionFound();
		}
	}
	
	private void bkt() throws SearchLimitException {
		totBkts++;
		restartBkts++;
		if (restartBkts >= restartLimit && solFound) {
			throw new SearchLimitException("lns restart");
		}
		if (isTimeout()) {
			throw new SearchLimitException("timeout");
		}
	}


	public void addRestartObserver(RestartObserver obs) {
		restartObservers.add(obs);
	}
	
	private void restartSolution() {
		for(RestartObserver obs: restartObservers) {
			obs.restart();
		}
	}

    /**
     *
     * @return the current number of backtracks achieved
     */
	public int getNbBkts() {
		return totBkts;
	}

    /**
     *
     * @return the current number of restart achieved
     */
	public int getNbRestarts() {
		return totRestarts;
	}
	
	private void restart() {
		node.popAll();
		node.pushState();
		if (!node.isFailed())
			restartSolution();
		else {
			System.out.println("space failed so don't restart");
		}
	}



    /**
     * perform a dfs on the reversible node to find all the solutions
     */
	public void solveAll() {
		startTime = System.currentTimeMillis();
		branching.initialize();
		boolean exhausted = true;
		try {
            node.pushState();
			solveAllRecur(currentNode);
            node.pop();
		} catch (SearchLimitException e) {
			System.out.print("!");
			exhausted = false;
		}
		if (exhausted && !restartObservers.isEmpty()) {
			System.out.print("R");
		}
		if (!isTimeout() && !stopRestarts) {
            restart();
        }
        else System.out.println("stopped by timeout");
	}
	
    /**
     * perform a dfs on the reversible node to find all the solutions
     */
	public void solveLNS(int maxNbRestart, int failure, Restart restart) {		
		totRestarts = 0;
		if (! node.hasObjective()) throw new RuntimeException("an objective must be defined to use lns");
		restartLimit = failure;
		this.restart = restart;

		startTime = System.currentTimeMillis();
		branching.initialize();

		for (int r = 0; r < maxNbRestart && !stopRestarts && !isTimeout(); r++) {

			boolean exhausted = true;
			try {
				node.pushState();
				solveAllRecur(currentNode);
				node.pop();
			} catch (SearchLimitException e) {
				System.out.print("!");
				exhausted = false;
			}
			if (exhausted) {
				System.out.print("R");
			}
			if (!isTimeout() && !stopRestarts) {
				node.popAll();
				node.pushState();
				if (!node.isFailed()) {
					if (!node.getObjective().isOptimum()) { //restart only if not at the optimum value of the objective
						
						restartBkts = 0;
						restart.restart(); // make the relaxation
					} else {
						System.out.println("optimum wrt to root node lower bound");
						stopRestarts = true;
					}
				} else {
					System.out.println("space failed so don't restart");
					stopRestarts = true;
				}
			}
			totRestarts++;
		}
		if (isTimeout()) {
			System.out.println("stopped by timeout");
		}
	}	

    /**
     * stop the restart process
     */
    public void stopRestarts() {
        stopRestarts = true;
    }
	
	
	private void solveAllRecur(int parentNode) throws SearchLimitException {
		Alternative [] alt = branching.getAlternatives();
		if (alt == null || alt.length == 0) {
			if (!node.isFailed()) {
				notifySolution();
				if (node.hasObjective()) {
					node.getObjective().tighten();
				}
			}
		} else {
			if (alt.length == 1) {
				currentNode = currentNode+1;
				if (alt[0].execute()) {
					solveAllRecur(currentNode);
				}
			}
			else {
				for (Alternative a : alt) {
					currentNode = currentNode+1;
					if (!node.isFailed()) {
						node.pushState();
						if (a.execute()){
							solveAllRecur(currentNode);
						}
						bkt(); //increment the number of backtrack
						node.pop();
                        a.executeOnBacktrack();
					}
				}
			}
		}
	}
		

    /**
     * perform a dfs on the reversible node to find the first feasible solution
     * @return  false if the search was stopped by a search limit, true otherwise
     */
	public boolean solveOne() {
		startTime = System.currentTimeMillis();
		branching.initialize();
		try {
			solveOneRecur();
		} catch (SearchLimitException e) {
			System.out.println("search limit reached");
			return false;
		}
		return true;
	}
	
	private boolean solveOneRecur() throws SearchLimitException{
		Alternative [] alt = branching.getAlternatives();
		if (alt==null || alt.length == 0) {
			if (!node.isFailed()) {
				notifySolution();
				return true;
			} else {
				return false;
			}
		} else {
			if (alt.length == 1) {
				if ( alt[0].execute() && solveOneRecur()) {
					return true;
				}
			}
			else {
				for (Alternative a : alt) {
					if (!node.isFailed()) {
						node.pushState();
						if (a.execute() && solveOneRecur()) {
							return true;
						}
						else {
							bkt();
							node.pop();
							if (!a.executeOnBacktrack()) {
								return false;
							}		
						}
					}
				}
			}
			return false; //none of the alternative succeeded
		}
	}
}
