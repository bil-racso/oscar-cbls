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
package oscar.cp.constraints;

import oscar.cp.core.CPVarInt;
import oscar.cp.scheduling.Activity;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class ThetaTree {



	private Node [] nodes;
	private int isize; // number of internal nodes
	private int size;

	public ThetaTree(int size) {
		// http://en.wikipedia.org/wiki/Binary_heap#Adding_to_the_heap
		this.size = size;
		isize = 1;
		//enumerate multiples of two 2, 4, 6, 8 ... until isize larger than size
		while (isize < size) {
			isize <<= 1; //shift the pattern to the left by 1 (i.e. multiplies by 2)
		}	
		//number of nodes in a complete  binary tree with isize leaf nodes is (isize * 2) - 1
		nodes = new Node[(isize << 2) - 1];
		for (int i = 0; i < nodes.length; i++) {
			nodes[i] = new Node();
		}
		isize--;
	}
	
	
	public void reset() {
		for (Node n: nodes) {
			n.reset();
		}
	}
	
	public void insert(Activity act, int pos) {
		// the last size nodes are the leaf nodes so the first one is isize (the number of internal nodes)
		int curr_pos = isize + pos; 
		Node node = nodes[curr_pos];
		node.setActivity(act);
		node.setECT(act.ect());
		node.setSUMP(act.minDuration());
		reCompute(getFather(curr_pos));
	}

	public void remove(int pos) {
		int curr_pos = isize + pos;
		Node node = nodes[curr_pos];
		node.reset();
		reCompute(getFather(curr_pos));
	}

	public int ect(int pos) {
		return nodes[pos].ect();
	}

	public int ect() {
		return ect(0);
	}
	
	int getSUMP(int pos) { 
		return nodes[pos].getSUMP(); 
	}

	boolean isInserted(int pos) {
		return nodes[pos + isize].hasActivity();
	}

	public int getFather(int pos) {
		//the father of node in pos is (pos-1)/2
		return (pos - 1) >> 1; 
	}

	public int getLeft(int pos) {
		//the left child of pos is pos*2+1
		return (pos << 1) + 1; 
	}

	public int getRight(int pos) {
		//the right child of pos is (pos+1)*2
		return (pos + 1) << 1;
	}

	private void reComputeAux(int pos) {
		int pl = getSUMP(getLeft(pos));
		int pr = getSUMP(getRight(pos));
		nodes[pos].setECT(pl + pr);

		int el = ect(getLeft(pos));
		int er = ect(getRight(pos));
		int en = Math.max(er, el + pr);
		nodes[pos].setECT(en);
	}


	private void reCompute(int pos) {
		while (pos > 0) {
			reComputeAux(pos);
			pos = getFather(pos);
		}
		// Fast recompute the top node. We do not need all info.
		nodes[0].setECT(Math.max(nodes[2].ect(),
				nodes[1].ect() + nodes[2].getSUMP()));
	}


}

class Node {

	private Activity act;
	private int sump;
	private int ect;

	public Node() {
		reset();
	}
	
	public void reset() {
		setActivity(null);
		setECT(Integer.MIN_VALUE);
		setSUMP(0);
	}
	
	public boolean hasActivity() {
		return act != null;
	}

	public Activity getActivity() {
		return act;
	}

	public int ect() {
		return ect;
	}

	public int getSUMP() {
		return sump;
	}

	public void setActivity(Activity act) {
		this.act = act;
	}

	public void setECT(int ect) {
		this.ect = ect;
	}

	public void setSUMP(int sump) {
		this.sump = sump;
	}

}







