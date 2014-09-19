/*
 * Main author(s) : Jean-Noël Monette
 * 
 * Copyright (c) 2012, Jean-Noël Monette
 * All rights reserved.

 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package oscar.flatzinc.parser;

import java.io.FileInputStream;
import java.io.IOException;
import oscar.flatzinc.ParsingException;
import org.antlr.v4.runtime.*;


public class FZParser {
	public static Model readFlatZincModelFromFile(String fname){
		try{
		ANTLRInputStream input = new ANTLRInputStream(new FileInputStream(fname));
		//UnbufferedCharStream input = new UnbufferedCharStream(new FileInputStream(fname));
		return readFlatZincModel(input);
		}catch(IOException e){
	        System.err.println("File " + fname + " not found. Aborting.");
			System.exit(1);
		}
		return null;
	}
	public static Model readFlatZincModelFromString(String content){
		ANTLRInputStream input = new ANTLRInputStream(content);
		return readFlatZincModel(input);
	}
	public static Model readFlatZincModel(CharStream input){
		Model m = new Model();
		try{
	        FlatzincLexer lex = new FlatzincLexer(input);
			CommonTokenStream tokens = new CommonTokenStream(lex);
			FlatzincParser p = new FlatzincParser(tokens, m);
			p.setBuildParseTree(false);//in order to get acceptable performance on large files
	        p.flatzinc_model();
	        if(p.getNumberOfSyntaxErrors()>0){
	        	System.err.println("Syntax error. Aborting.");
	            System.err.println("If the flatzinc file is correct, please report to the developers.");
	        	System.exit(1);
	        }
			return m;
		}catch(RecognitionException e){
			e.printStackTrace();//TODO: report more friendly messages
			System.err.println("Syntax error. Aborting.");
	          System.err.println("If the flatzinc file is correct, please report to the developers.");
			System.exit(1);
		}catch(ParsingException e){
		  e.printStackTrace();//TODO: report more friendly messages
          System.err.println("Syntax error. Aborting.");
          System.err.println("If the flatzinc file is correct, please report to the developers.");
          System.exit(1);
		}
		return null;
	}
}
