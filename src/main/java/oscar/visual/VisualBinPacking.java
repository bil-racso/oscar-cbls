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
package oscar.visual;

import java.awt.Color;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Random;

import javax.swing.JInternalFrame;
import javax.swing.JPanel;

/**
 * 
 * @author Pierre Schaus
 *
 */
public class VisualBinPacking extends VisualDrawing {

	private int nbBins = 0;
	private int binWidth = 10;
	private ArrayList<VisualBinPackingItem> items = new ArrayList<VisualBinPacking.VisualBinPackingItem>();
	
	public VisualBinPacking(int nbBins, int binWidth) {
		super(false,true);
		this.nbBins = nbBins;
		this.binWidth = binWidth;
	}
	
	public VisualBinPackingItem addItem(int bin, int height) {
		VisualBinPackingItem item = new VisualBinPackingItem(this, bin, height);
		item.setInnerCol(VisualUtil.getRandomColor());
		items.add(item);
		item.setBin(bin);
		return item;
	}
	
	private void redraw(int bin) {
		int y = 0;
		for (VisualBinPackingItem item: items) {
			if (item.bin == bin) {
				item.move(bin*binWidth, y);
				y+= item.getHeight();
			}
		}
	}
	
	
	
	
	public class VisualBinPackingItem extends VisualRectangle {

		int bin = 0; 
		private VisualBinPackingItem(VisualDrawing d, int bin, int height) {
			super(d, 0, 0, binWidth, height);
		}
		
		public void setBin(int bin) {
			int oldBin = this.bin;
			this.bin = bin;
			move(binWidth*bin, getY());
			redraw(oldBin);
			redraw(bin);
		}
		
	}
	
	public static void main(String[] args) {


			VisualFrame f = new VisualFrame("toto");
			VisualBinPacking bp = new VisualBinPacking(10, 50);
			
			JInternalFrame inf = f.createFrame("Drawing");
			inf.add(bp);
			f.pack();
			
			VisualBinPackingItem [] items = new VisualBinPackingItem[10];
			Random rand = new Random();
			
			for (int i = 0; i < items.length; i++) {
				items[i] = bp.addItem(i, (int)(Math.random()*100));
			}
			
			for (int i = 0; i < 100; i++) {
				
				items[rand.nextInt(items.length)].setBin(rand.nextInt(10));
				
				try {
					Thread.sleep(500);
				} catch (InterruptedException e) {
				}
			}
			
			

			
			


	}	

}
