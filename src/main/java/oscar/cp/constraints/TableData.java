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

import java.util.ArrayList;
import java.util.Arrays;

/**
 * Table Data used to build a table constraint and specify the acceptable tuples
 * @author pschaus@gmail.com
 */
public class TableData {

	int arity;

	ArrayList<Integer> [] data;

	int [][] firstSupport;
	int [][] nextSupport;
	int [] min;
	int [] max;
	
	int [][] tuples = null;

	/**
	 *
	 * @param arity of the tupples than can be added to this table
	 */
	@SuppressWarnings("unchecked")
	public TableData(int arity){
		this.arity = arity;

		data = new ArrayList[arity];
		for (int i = 0; i < data.length; i++) {
			data[i] = new ArrayList<Integer>();
		}

		firstSupport = new int [arity] [];
		nextSupport = new int [arity] [];
		min = new int[arity];
		max = new int [arity];


	}
	
	/**
	 * @return one array entry per tuple
	 */
	public int [][] getTuples() {
		if (tuples != null) return tuples;
		int [][] res = new int[data[0].size()][arity];
		for (int i = 0; i < res.length; i++) {
			for (int a = 0; a < arity; a++) {
				res[i][a] = data[a].get(i);
			}
		}
		tuples = res;
		return tuples;
	}

	protected int getMin(int i){
		return min[i];
	}

	protected int getMax(int i){
		return max[i];
	}

	/**
	 * Adds a possible tuple
	 * @param tuple must have a length arity
	 */	
	public void addTuple(int ...tuple){
		assert(arity == tuple.length);
		for (int i = 0; i < tuple.length; i++) {
			data[i].add(tuple[i]);
		}	
	}

	protected int getValue(int tuple, int index){
		return data[index].get(tuple);
	}

	protected void printTuple(int tuple){
		for (int i = 0; i < arity; i++) {
			System.out.print(data[i].get(tuple)+" ");
		} System.out.println();
	}

	protected void setUp(){
		for (int i = 0; i < arity; i++) {
			buildSupport(i);
		}
	}

	protected void buildSupport(int i){
		int mini = Integer.MAX_VALUE;
		int maxi = Integer.MIN_VALUE;
		for(int j = 0; j < data[i].size(); j++) {
			if (data[i].get(j) < mini) {
				mini = data[i].get(j);
			}
			if (data[i].get(j) > maxi) {
				maxi = data[i].get(j);
			}
		}
		min[i] = mini;
		max[i] = maxi;

		firstSupport[i] = new int [maxi-mini+1];
		for (int j = 0; j < firstSupport[i].length; j++) {
			firstSupport[i][j] = -1;
		}
		nextSupport[i] = new int[data[i].size()];
		for (int j = 0; j < nextSupport[i].length; j++) {
			nextSupport[i][j] = -1;
		}

		for (int j = 0; j < data[i].size(); j++) {
			int v = data[i].get(j);
			nextSupport[i][j] = firstSupport[i][v-min[i]];
			firstSupport[i][v-min[i]] = j;
		}	
	}

	protected int getFirstSupport(int i,int value){
		return firstSupport[i][value-min[i]];
	}

	protected boolean hasFirstSupport(int i,int value){
		return firstSupport[i][value-min[i]] != -1;
	}

	protected boolean hasNextSupport(int i,int tuple){
		if (tuple < 0) {
			return false;
		}
		return nextSupport[i][tuple] != -1;
	}

	protected int getNextSupport(int i,int tuple){
		if (tuple < 0) {
			return tuple;
		}
		return nextSupport[i][tuple];
	}
	
	@Override
	public String toString() {
		String res = "";
		for (int i = 0; i < data[0].size(); i++) {
			res += "( ";
			for (ArrayList<Integer> col : data) {
				res += col.get(i)+ " ";
			}
			res += ") , ";
		}
		return res;	
	}


}
