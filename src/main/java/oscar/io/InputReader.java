/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package oscar.io;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.StringTokenizer;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class InputReader {
	
	private BufferedReader in;
	private StringTokenizer tokenizer;
	
	public InputReader(String file) {
		try {
			
			FileInputStream istream = new FileInputStream(file);
			in = new BufferedReader(new InputStreamReader(istream));
			tokenizer = new StringTokenizer("");
		} catch (Exception e) {
			e.printStackTrace();
		} 
	}
	
	public Integer getInt() throws RuntimeException{
		if (!tokenizer.hasMoreTokens()) {
			try {
				String line;
				do {
					line = in.readLine();
					if (line == null) {
						System.out.println("no more line to read");
						throw new RuntimeException("End of file");
					}
					tokenizer = new StringTokenizer(line);
				} while (line != null && !tokenizer.hasMoreTokens());

			} catch (IOException e) {
				throw new RuntimeException(e.toString());
			}
		}
		return Integer.parseInt(tokenizer.nextToken());
	}
	
	public String getString() throws RuntimeException{
		if (!tokenizer.hasMoreTokens()) {
			try {
				String line;
				do {
					line = in.readLine();
					if (line == null) {
						System.out.println("no more line to read");
						throw new RuntimeException("End of file");
					}
					tokenizer = new StringTokenizer(in.readLine());
				} while (line != null && !tokenizer.hasMoreTokens());

			} catch (IOException e) {
				throw new RuntimeException(e.toString());
			}
		}
		return tokenizer.nextToken();
	}

}
