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
package oscar.cp.constraints.implementations;

import oscar.cp.scheduling.Activity;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class LambdaThetaTree {



	private LTNode [] nodes;
	private int isize; //number of internal nodes
	private int size;

	public LambdaThetaTree(int size) {
		// http://en.wikipedia.org/wiki/Binary_heap#Adding_to_the_heap
		this.size = size;
		isize = 1;
		//enumerate mupltiples of two 2, 4, 6, 8 ... until isize larger than size
		while (isize < size) {
			isize <<= 1; //shift the pattern to the left by 1 (i.e. multiplies by 2)
			//System.out.println(isize);
		}	
		//number of nodes in a complete  binary tree with isize leaf nodes is (isize*2)-1
		nodes = new LTNode[(isize << 2) -1];
		for (int i = 0; i < nodes.length; i++) {
			nodes[i] = new LTNode();
		}
		isize--;
	}
	
	
	public void reset() {
		for (LTNode n: nodes) {
			n.reset();
		}
	}
	
	public void insert(Activity act, int pos) {
		//the last size nodes are the leaf nodes so the first one is isize (the number of internal nodes)
		int curr_pos = isize + pos; 
		LTNode node = nodes[curr_pos];
		node.setActivity(act);
		node.setECT(act.ect());
		node.setSUMP(act.minDuration());
		node.setECT_OPT(act.ect());
		node.setSUMP_OPT(act.minDuration());
		node.setResponsible_ECT(-1);
		node.setResponsible_SUMP(-1);
		System.out.println("insert into node "+curr_pos+":"+node);
		reCompute(getFather(curr_pos));
	}
	
	public void grey(int pos) {
		int curr_pos = isize + pos; 
		LTNode node = nodes[curr_pos];
		node.setECT(Integer.MIN_VALUE);
		node.setSUMP(0);
		node.setResponsible_ECT(pos);
		node.setResponsible_SUMP(pos);
		reCompute(getFather(curr_pos));
	}

	public void remove(int pos) {
		int curr_pos = isize + pos;
		LTNode node = nodes[curr_pos];
		node.reset();
		reCompute(getFather(curr_pos));
	}

	private int ect(int pos) {
		return nodes[pos].ect();
	}

	public int ect() {
		return ect(0);
	}
	
	private int getECT_OPT(int pos) {
		return nodes[pos].getECT_OPT();
	}

	public int getECT_OPT() { 
		return getECT_OPT(0); 
	}
	
	private int getSUMP_OPT(int pos) {
		return nodes[pos].getSUMP_OPT();
	}

	public int getSUM_OPT() { 
		return getSUMP_OPT(0); 
	}
	
	private int getResponsible_ECT(int pos) {
		return nodes[pos].getResponsible_ECT(); 
	}
	
	public int getResponsible_ECT() {
		return getResponsible_ECT(0); 
	}
	
	private int getResponsible_SUMP(int pos) {
		return nodes[pos].getResponsible_SUMP(); 
	}
	
	private int getSUMP(int pos) { 
		return nodes[pos].getSUMP(); 
	}

	public boolean isInserted(int pos) {
		return nodes[pos + isize].hasActivity();
	}

	private int getFather(int pos) {
		//the father of node in pos is (pos-1)/2
		return (pos - 1) >> 1; 
	}

	private int getLeft(int pos) {
		//the left child of pos is pos*2+1
		return (pos << 1) + 1; 
	}

	private int getRight(int pos) {
		//the right child of pos is (pos+1)*2
		return (pos + 1) << 1;
	}

	private void reComputeAux(int pos) {
		
		
		LTNode n = nodes[pos];
		int pr = getSUMP(getRight(pos));
		n.setSUMP(getSUMP(getLeft(pos)) + pr);
		n.setECT(Math.max(ect((getRight(pos))), ect(getLeft(pos)) + pr));
		
		
		if (getResponsible_ECT(getLeft(pos)) == -1 && getResponsible_ECT(getRight(pos)) == -1) {
			n.setSUMP_OPT(n.getSUMP());
			n.setECT_OPT(n.ect());
			n.setResponsible_ECT(-1);
			n.setResponsible_SUMP(-1);
		} else {
			int lo = getSUMP_OPT(getLeft(pos)) + getSUMP(getRight(pos));
			int ro = getSUMP(getLeft(pos)) + getSUMP_OPT(getRight(pos));
			if (lo > ro) {
				n.setSUMP_OPT(lo);
				n.setResponsible_SUMP(getResponsible_SUMP(getLeft(pos)));
			} else {
				n.setSUMP_OPT(ro);
				n.setResponsible_SUMP(getResponsible_SUMP(getRight(pos)));
			}
			int ect1 = getECT_OPT(getRight(pos));
			int ect2 = ect(getLeft(pos)) + getSUMP_OPT(getRight(pos));
			int ect3 = getECT_OPT(getLeft(pos)) + getSUMP(getRight(pos));
			if (ect1 >= ect2 && ect1 >= ect3) {  // ect1 max
				n.setECT_OPT(ect1);
				n.setResponsible_ECT(getResponsible_ECT(getRight(pos)));
			} else if (ect2 >= ect1 && ect2 >= ect3) {  // ect2 max
				n.setECT_OPT(ect2);
				n.setResponsible_ECT(getResponsible_SUMP(getRight(pos)));
			} else {  // ect3 max
				n.setECT_OPT(ect3);
				n.setResponsible_ECT(getResponsible_ECT(getLeft(pos)));
			}
			assert(n.getResponsible_SUMP() != -1);
		}
	}
	
	public void reComputeTop() {
		LTNode n = nodes[0];
		n.setECT(Math.max(ect(2), ect(1) + getSUMP(2)));
		if (getResponsible_ECT(1) == -1 && getResponsible_ECT(2) == -1) {
			n.setSUMP_OPT(n.getSUMP());
			n.setECT_OPT(n.ect());
			n.setResponsible_ECT(-1);
			n.setResponsible_SUMP(-1);
		} else {
			int ect1 = getECT_OPT(2);
			int ect2 = ect(1) + getSUMP_OPT(2);
			int ect3 = getECT_OPT(1) + getSUMP(2);
			if (ect1 >= ect2 && ect1 >= ect3) {
				n.setECT_OPT(ect1);
				n.setResponsible_ECT(getResponsible_ECT(2));
			} else if (ect2 >= ect1 && ect2 >= ect3) {
				n.setECT_OPT(ect2);
				n.setResponsible_ECT(getResponsible_SUMP(2));
			} else {  // ect3 >= ect1 && ect3 >= ect2
				n.setECT_OPT(ect3);
				n.setResponsible_ECT(getResponsible_ECT(1));
			}
		}
	}

	private void reCompute(int pos) {
		while (pos > 0) {
			reComputeAux(pos);
			pos = getFather(pos);
		}
		reComputeTop();
	}

}

class LTNode {

	private Activity act;
	private int sump;
	private int ect;
	
	private int sump_opt;
    private int ect_opt;
    private int responsible_ect;
    private int responsible_sump;
    

	public LTNode() {
		reset();
	}
	
	public void reset() {
		setActivity(null);
		setECT(Integer.MIN_VALUE);
		setSUMP(0);
		setECT_OPT(Integer.MIN_VALUE);
		setSUMP_OPT(0);
		setResponsible_ECT(-1);
		setResponsible_SUMP(-1);
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

	public int getSUMP_OPT() {
		return sump_opt;
	}

	public void setSUMP_OPT(int sump_opt) {
		this.sump_opt = sump_opt;
	}

	public int getECT_OPT() {
		return ect_opt;
	}

	public void setECT_OPT(int ect_opt) {
		this.ect_opt = ect_opt;
	}

	public int getResponsible_ECT() {
		return responsible_ect;
	}

	public void setResponsible_ECT(int responsible_ect) {
		this.responsible_ect = responsible_ect;
	}

	public int getResponsible_SUMP() {
		return responsible_sump;
	}

	public void setResponsible_SUMP(int responsible_sump) {
		this.responsible_sump = responsible_sump;
	}
	
	public String toString() {
		
		return "---LTNode--------\n"+
			   "sump:"+getSUMP()+"\n"+
			   "sump_opt:"+getSUMP_OPT()+"\n"+
			   "resp_sump:"+getResponsible_SUMP()+"\n"+
			   "ect:"+ect()+"\n"+
			   "ect_opt:"+getECT_OPT()+"\n"+
			   "resp_ect:"+getResponsible_ECT()+"\n"+
			   "est:"+act.est();
			   
	}
	


}







