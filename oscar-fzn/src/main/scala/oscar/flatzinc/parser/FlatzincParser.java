// Generated from /home/janho/workspace/oscar/oscar-fzn/src/main/scala/oscar/flatzinc/parser/Flatzinc.g by ANTLR 4.4

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
package oscar.flatzinc.parser;
import oscar.flatzinc.parser.intermediatemodel.*;
import oscar.flatzinc.model.Annotation;
import oscar.flatzinc.model.Domain;
import oscar.flatzinc.model.DomainSet;
import oscar.flatzinc.model.DomainRange;
import oscar.flatzinc.ParsingException;
import java.util.Set;
import java.util.HashSet;


import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class FlatzincParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.4", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__24=1, T__23=2, T__22=3, T__21=4, T__20=5, T__19=6, T__18=7, T__17=8, 
		T__16=9, T__15=10, T__14=11, T__13=12, T__12=13, T__11=14, T__10=15, T__9=16, 
		T__8=17, T__7=18, T__6=19, T__5=20, T__4=21, T__3=22, T__2=23, T__1=24, 
		T__0=25, Boolconst=26, PREDANNID=27, VARPARID=28, Floatconst=29, INT=30, 
		STRING=31, WS=32;
	public static final String[] tokenNames = {
		"<INVALID>", "']'", "'minimize'", "'of'", "','", "'['", "'('", "':'", 
		"'int'", "'array'", "'var'", "'set'", "'{'", "'}'", "'float'", "'predicate'", 
		"'bool'", "')'", "'::'", "'constraint'", "'='", "';'", "'maximize'", "'solve'", 
		"'satisfy'", "'..'", "Boolconst", "PREDANNID", "VARPARID", "Floatconst", 
		"INT", "STRING", "WS"
	};
	public static final int
		RULE_flatzinc_model = 0, RULE_preddecl = 1, RULE_predparam = 2, RULE_paramdecl = 3, 
		RULE_vardecl = 4, RULE_constraint = 5, RULE_solvegoal = 6, RULE_predparamtype = 7, 
		RULE_basicpartype = 8, RULE_partype = 9, RULE_parpredparamtype = 10, RULE_basicparpredparamtype = 11, 
		RULE_vartype = 12, RULE_basicvartype = 13, RULE_varpredparamtype = 14, 
		RULE_arraytype = 15, RULE_expr = 16, RULE_setconst = 17, RULE_arrayexpr = 18, 
		RULE_annotations = 19, RULE_annotation = 20, RULE_predannid = 21, RULE_boolconst = 22, 
		RULE_floatconst = 23, RULE_intconst = 24, RULE_stringconstant = 25, RULE_varparid = 26;
	public static final String[] ruleNames = {
		"flatzinc_model", "preddecl", "predparam", "paramdecl", "vardecl", "constraint", 
		"solvegoal", "predparamtype", "basicpartype", "partype", "parpredparamtype", 
		"basicparpredparamtype", "vartype", "basicvartype", "varpredparamtype", 
		"arraytype", "expr", "setconst", "arrayexpr", "annotations", "annotation", 
		"predannid", "boolconst", "floatconst", "intconst", "stringconstant", 
		"varparid"
	};

	@Override
	public String getGrammarFileName() { return "Flatzinc.g"; }

	@Override
	public String[] getTokenNames() { return tokenNames; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }


		private Model m;
		public FlatzincParser(TokenStream input,Model m){
			this(input);
			this.m = m;
		}

	public FlatzincParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class Flatzinc_modelContext extends ParserRuleContext {
		public ConstraintContext constraint(int i) {
			return getRuleContext(ConstraintContext.class,i);
		}
		public List<ConstraintContext> constraint() {
			return getRuleContexts(ConstraintContext.class);
		}
		public ParamdeclContext paramdecl(int i) {
			return getRuleContext(ParamdeclContext.class,i);
		}
		public PreddeclContext preddecl(int i) {
			return getRuleContext(PreddeclContext.class,i);
		}
		public List<PreddeclContext> preddecl() {
			return getRuleContexts(PreddeclContext.class);
		}
		public VardeclContext vardecl(int i) {
			return getRuleContext(VardeclContext.class,i);
		}
		public List<ParamdeclContext> paramdecl() {
			return getRuleContexts(ParamdeclContext.class);
		}
		public SolvegoalContext solvegoal() {
			return getRuleContext(SolvegoalContext.class,0);
		}
		public List<VardeclContext> vardecl() {
			return getRuleContexts(VardeclContext.class);
		}
		public Flatzinc_modelContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_flatzinc_model; }
	}

	public final Flatzinc_modelContext flatzinc_model() throws RecognitionException {
		Flatzinc_modelContext _localctx = new Flatzinc_modelContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_flatzinc_model);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(57);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__10) {
				{
				{
				setState(54); preddecl();
				}
				}
				setState(59);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(63);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,1,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(60); paramdecl();
					}
					} 
				}
				setState(65);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,1,_ctx);
			}
			setState(69);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__16 || _la==T__15) {
				{
				{
				setState(66); vardecl();
				}
				}
				setState(71);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(75);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__6) {
				{
				{
				setState(72); constraint();
				}
				}
				setState(77);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(78); solvegoal();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PreddeclContext extends ParserRuleContext {
		public TerminalNode PREDANNID() { return getToken(FlatzincParser.PREDANNID, 0); }
		public PredparamContext predparam(int i) {
			return getRuleContext(PredparamContext.class,i);
		}
		public List<PredparamContext> predparam() {
			return getRuleContexts(PredparamContext.class);
		}
		public PreddeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_preddecl; }
	}

	public final PreddeclContext preddecl() throws RecognitionException {
		PreddeclContext _localctx = new PreddeclContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_preddecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(80); match(T__10);
			setState(81); match(PREDANNID);
			setState(82); match(T__19);
			setState(83); predparam();
			setState(88);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__21) {
				{
				{
				setState(84); match(T__21);
				setState(85); predparam();
				}
				}
				setState(90);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(91); match(T__8);
			setState(92); match(T__4);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PredparamContext extends ParserRuleContext {
		public PredparamtypeContext predparamtype() {
			return getRuleContext(PredparamtypeContext.class,0);
		}
		public PredannidContext predannid() {
			return getRuleContext(PredannidContext.class,0);
		}
		public PredparamContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_predparam; }
	}

	public final PredparamContext predparam() throws RecognitionException {
		PredparamContext _localctx = new PredparamContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_predparam);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(94); predparamtype();
			setState(95); match(T__18);
			setState(96); predannid();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ParamdeclContext extends ParserRuleContext {
		public PartypeContext partype;
		public VarparidContext varparid;
		public ExprContext expr;
		public PartypeContext partype() {
			return getRuleContext(PartypeContext.class,0);
		}
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public VarparidContext varparid() {
			return getRuleContext(VarparidContext.class,0);
		}
		public ParamdeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_paramdecl; }
	}

	public final ParamdeclContext paramdecl() throws RecognitionException {
		ParamdeclContext _localctx = new ParamdeclContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_paramdecl);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(98); ((ParamdeclContext)_localctx).partype = partype();
			setState(99); match(T__18);
			setState(100); ((ParamdeclContext)_localctx).varparid = varparid();
			setState(101); match(T__5);
			setState(102); ((ParamdeclContext)_localctx).expr = expr();
			setState(103); match(T__4);
			Element e = ((ParamdeclContext)_localctx).expr.e;
				e.name = ((ParamdeclContext)_localctx).varparid.text;
				e.typ = ((ParamdeclContext)_localctx).partype.t;
				m.addId(e.name,e);
				//TODO: Check that expr is a boolconst, floatconst, intconst, setconst, or an array thereof.
				
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class VardeclContext extends ParserRuleContext {
		public Element e;
		public VartypeContext vartype;
		public VarparidContext varparid;
		public AnnotationsContext annotations;
		public ExprContext expr;
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public VarparidContext varparid() {
			return getRuleContext(VarparidContext.class,0);
		}
		public VartypeContext vartype() {
			return getRuleContext(VartypeContext.class,0);
		}
		public AnnotationsContext annotations() {
			return getRuleContext(AnnotationsContext.class,0);
		}
		public VardeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_vardecl; }
	}

	public final VardeclContext vardecl() throws RecognitionException {
		VardeclContext _localctx = new VardeclContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_vardecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(106); ((VardeclContext)_localctx).vartype = vartype();
			setState(107); match(T__18);
			setState(108); ((VardeclContext)_localctx).varparid = varparid();
			setState(109); ((VardeclContext)_localctx).annotations = annotations();
			setState(114);
			_la = _input.LA(1);
			if (_la==T__5) {
				{
				setState(110); match(T__5);
				setState(111); ((VardeclContext)_localctx).expr = expr();
				((VardeclContext)_localctx).e =  ((VardeclContext)_localctx).expr.e;
				}
			}

			setState(116); match(T__4);
			if(_localctx.e==null){
				  m.addNewVariable(((VardeclContext)_localctx).vartype.t,((VardeclContext)_localctx).vartype.d,((VardeclContext)_localctx).varparid.text, ((VardeclContext)_localctx).annotations.anns);
				}else{
				  m.addAliasVariable(((VardeclContext)_localctx).vartype.t,((VardeclContext)_localctx).vartype.d,((VardeclContext)_localctx).varparid.text, _localctx.e, ((VardeclContext)_localctx).annotations.anns);
				}
				// TODO: Check that Any vars in assignments must be declared earlier.
				
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ConstraintContext extends ParserRuleContext {
		public List<Element> args;
		public PredannidContext predannid;
		public ExprContext e;
		public ExprContext e1;
		public AnnotationsContext annotations;
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public PredannidContext predannid() {
			return getRuleContext(PredannidContext.class,0);
		}
		public AnnotationsContext annotations() {
			return getRuleContext(AnnotationsContext.class,0);
		}
		public ConstraintContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_constraint; }
	}

	public final ConstraintContext constraint() throws RecognitionException {
		ConstraintContext _localctx = new ConstraintContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_constraint);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(119); match(T__6);
			setState(120); ((ConstraintContext)_localctx).predannid = predannid();
			setState(121); match(T__19);
			setState(122); ((ConstraintContext)_localctx).e = expr();
			((ConstraintContext)_localctx).args =  new ArrayList<Element>(); _localctx.args.add(((ConstraintContext)_localctx).e.e);
			setState(130);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__21) {
				{
				{
				setState(124); match(T__21);
				setState(125); ((ConstraintContext)_localctx).e1 = expr();
				_localctx.args.add(((ConstraintContext)_localctx).e1.e);
				}
				}
				setState(132);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(133); match(T__8);
			setState(134); ((ConstraintContext)_localctx).annotations = annotations();
			m.addConstraint(((ConstraintContext)_localctx).predannid.text,_localctx.args,((ConstraintContext)_localctx).annotations.anns);
			setState(136); match(T__4);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SolvegoalContext extends ParserRuleContext {
		public AnnotationsContext annotations;
		public ExprContext expr;
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public AnnotationsContext annotations() {
			return getRuleContext(AnnotationsContext.class,0);
		}
		public SolvegoalContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_solvegoal; }
	}

	public final SolvegoalContext solvegoal() throws RecognitionException {
		SolvegoalContext _localctx = new SolvegoalContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_solvegoal);
		try {
			setState(158);
			switch ( getInterpreter().adaptivePredict(_input,7,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(138); match(T__2);
				setState(139); ((SolvegoalContext)_localctx).annotations = annotations();
				setState(140); match(T__1);
				setState(141); match(T__4);
				m.setSATObjective(((SolvegoalContext)_localctx).annotations.anns);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(144); match(T__2);
				setState(145); ((SolvegoalContext)_localctx).annotations = annotations();
				setState(146); match(T__23);
				setState(147); ((SolvegoalContext)_localctx).expr = expr();
				setState(148); match(T__4);
				m.setMINObjective(((SolvegoalContext)_localctx).expr.e,((SolvegoalContext)_localctx).annotations.anns);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(151); match(T__2);
				setState(152); ((SolvegoalContext)_localctx).annotations = annotations();
				setState(153); match(T__3);
				setState(154); ((SolvegoalContext)_localctx).expr = expr();
				setState(155); match(T__4);
				m.setMAXObjective(((SolvegoalContext)_localctx).expr.e,((SolvegoalContext)_localctx).annotations.anns);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PredparamtypeContext extends ParserRuleContext {
		public ParpredparamtypeContext parpredparamtype() {
			return getRuleContext(ParpredparamtypeContext.class,0);
		}
		public VarpredparamtypeContext varpredparamtype() {
			return getRuleContext(VarpredparamtypeContext.class,0);
		}
		public PredparamtypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_predparamtype; }
	}

	public final PredparamtypeContext predparamtype() throws RecognitionException {
		PredparamtypeContext _localctx = new PredparamtypeContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_predparamtype);
		try {
			setState(162);
			switch ( getInterpreter().adaptivePredict(_input,8,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(160); parpredparamtype();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(161); varpredparamtype();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class BasicpartypeContext extends ParserRuleContext {
		public Type t;
		public BasicpartypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_basicpartype; }
	}

	public final BasicpartypeContext basicpartype() throws RecognitionException {
		BasicpartypeContext _localctx = new BasicpartypeContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_basicpartype);
		try {
			setState(174);
			switch (_input.LA(1)) {
			case T__9:
				enterOuterAlt(_localctx, 1);
				{
				setState(164); match(T__9);
				((BasicpartypeContext)_localctx).t =  new Type("bool");
				}
				break;
			case T__11:
				enterOuterAlt(_localctx, 2);
				{
				setState(166); match(T__11);
				((BasicpartypeContext)_localctx).t =  new Type("float");
				}
				break;
			case T__17:
				enterOuterAlt(_localctx, 3);
				{
				setState(168); match(T__17);
				((BasicpartypeContext)_localctx).t =  new Type("int");
				}
				break;
			case T__14:
				enterOuterAlt(_localctx, 4);
				{
				setState(170); match(T__14);
				setState(171); match(T__22);
				setState(172); match(T__17);
				((BasicpartypeContext)_localctx).t =  new Type("set");
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PartypeContext extends ParserRuleContext {
		public Type t;
		public BasicpartypeContext basicpartype;
		public ArraytypeContext arraytype;
		public ArraytypeContext arraytype() {
			return getRuleContext(ArraytypeContext.class,0);
		}
		public BasicpartypeContext basicpartype() {
			return getRuleContext(BasicpartypeContext.class,0);
		}
		public PartypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_partype; }
	}

	public final PartypeContext partype() throws RecognitionException {
		PartypeContext _localctx = new PartypeContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_partype);
		try {
			setState(183);
			switch (_input.LA(1)) {
			case T__17:
			case T__14:
			case T__11:
			case T__9:
				enterOuterAlt(_localctx, 1);
				{
				setState(176); ((PartypeContext)_localctx).basicpartype = basicpartype();
				((PartypeContext)_localctx).t =  ((PartypeContext)_localctx).basicpartype.t;
				}
				break;
			case T__16:
				enterOuterAlt(_localctx, 2);
				{
				setState(179); ((PartypeContext)_localctx).arraytype = arraytype();
				setState(180); ((PartypeContext)_localctx).basicpartype = basicpartype();
				((PartypeContext)_localctx).t =  ((PartypeContext)_localctx).basicpartype.t; (_localctx.t).isArray=true; (_localctx.t).size = ((PartypeContext)_localctx).arraytype.size;
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ParpredparamtypeContext extends ParserRuleContext {
		public Type t;
		public PartypeContext partype;
		public BasicparpredparamtypeContext basicparpredparamtype;
		public ArraytypeContext arraytype;
		public ArraytypeContext arraytype() {
			return getRuleContext(ArraytypeContext.class,0);
		}
		public PartypeContext partype() {
			return getRuleContext(PartypeContext.class,0);
		}
		public BasicparpredparamtypeContext basicparpredparamtype() {
			return getRuleContext(BasicparpredparamtypeContext.class,0);
		}
		public ParpredparamtypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_parpredparamtype; }
	}

	public final ParpredparamtypeContext parpredparamtype() throws RecognitionException {
		ParpredparamtypeContext _localctx = new ParpredparamtypeContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_parpredparamtype);
		try {
			setState(195);
			switch ( getInterpreter().adaptivePredict(_input,11,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(185); ((ParpredparamtypeContext)_localctx).partype = partype();
				((ParpredparamtypeContext)_localctx).t =  ((ParpredparamtypeContext)_localctx).partype.t;
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(188); ((ParpredparamtypeContext)_localctx).basicparpredparamtype = basicparpredparamtype();
				((ParpredparamtypeContext)_localctx).t =  ((ParpredparamtypeContext)_localctx).basicparpredparamtype.t;
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(191); ((ParpredparamtypeContext)_localctx).arraytype = arraytype();
				setState(192); ((ParpredparamtypeContext)_localctx).basicparpredparamtype = basicparpredparamtype();
				((ParpredparamtypeContext)_localctx).t =  ((ParpredparamtypeContext)_localctx).basicparpredparamtype.t; (_localctx.t).isArray=true; (_localctx.t).size = ((ParpredparamtypeContext)_localctx).arraytype.size;
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class BasicparpredparamtypeContext extends ParserRuleContext {
		public Type t;
		public FloatconstContext floatconst(int i) {
			return getRuleContext(FloatconstContext.class,i);
		}
		public List<FloatconstContext> floatconst() {
			return getRuleContexts(FloatconstContext.class);
		}
		public SetconstContext setconst() {
			return getRuleContext(SetconstContext.class,0);
		}
		public BasicparpredparamtypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_basicparpredparamtype; }
	}

	public final BasicparpredparamtypeContext basicparpredparamtype() throws RecognitionException {
		BasicparpredparamtypeContext _localctx = new BasicparpredparamtypeContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_basicparpredparamtype);
		try {
			setState(210);
			switch (_input.LA(1)) {
			case Floatconst:
				enterOuterAlt(_localctx, 1);
				{
				setState(197); floatconst();
				setState(198); match(T__0);
				setState(199); floatconst();
				((BasicparpredparamtypeContext)_localctx).t =  new Type("float");
				}
				break;
			case T__13:
			case INT:
				enterOuterAlt(_localctx, 2);
				{
				setState(202); setconst();
				((BasicparpredparamtypeContext)_localctx).t =  new Type("int");
				}
				break;
			case T__14:
				enterOuterAlt(_localctx, 3);
				{
				setState(205); match(T__14);
				setState(206); match(T__22);
				setState(207); setconst();
				((BasicparpredparamtypeContext)_localctx).t =  new Type("set");
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class VartypeContext extends ParserRuleContext {
		public Type t;
		public Element d;
		public BasicvartypeContext basicvartype;
		public ArraytypeContext arraytype;
		public ArraytypeContext arraytype() {
			return getRuleContext(ArraytypeContext.class,0);
		}
		public BasicvartypeContext basicvartype() {
			return getRuleContext(BasicvartypeContext.class,0);
		}
		public VartypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_vartype; }
	}

	public final VartypeContext vartype() throws RecognitionException {
		VartypeContext _localctx = new VartypeContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_vartype);
		try {
			setState(219);
			switch (_input.LA(1)) {
			case T__15:
				enterOuterAlt(_localctx, 1);
				{
				setState(212); ((VartypeContext)_localctx).basicvartype = basicvartype();
				((VartypeContext)_localctx).t =  ((VartypeContext)_localctx).basicvartype.t; ((VartypeContext)_localctx).d =  ((VartypeContext)_localctx).basicvartype.d;
				}
				break;
			case T__16:
				enterOuterAlt(_localctx, 2);
				{
				setState(215); ((VartypeContext)_localctx).arraytype = arraytype();
				setState(216); ((VartypeContext)_localctx).basicvartype = basicvartype();
				((VartypeContext)_localctx).t =  ((VartypeContext)_localctx).basicvartype.t; (_localctx.t).isArray=true; (_localctx.t).size = ((VartypeContext)_localctx).arraytype.size; ((VartypeContext)_localctx).d =  ((VartypeContext)_localctx).basicvartype.d;
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class BasicvartypeContext extends ParserRuleContext {
		public Type t;
		public Element d;
		public SetconstContext setconst;
		public FloatconstContext floatconst(int i) {
			return getRuleContext(FloatconstContext.class,i);
		}
		public List<FloatconstContext> floatconst() {
			return getRuleContexts(FloatconstContext.class);
		}
		public SetconstContext setconst() {
			return getRuleContext(SetconstContext.class,0);
		}
		public BasicvartypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_basicvartype; }
	}

	public final BasicvartypeContext basicvartype() throws RecognitionException {
		BasicvartypeContext _localctx = new BasicvartypeContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_basicvartype);
		try {
			setState(246);
			switch ( getInterpreter().adaptivePredict(_input,14,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(221); match(T__15);
				setState(222); match(T__9);
				((BasicvartypeContext)_localctx).t =  new Type("bool"); (_localctx.t).isVar = true;
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(224); match(T__15);
				setState(225); match(T__11);
				((BasicvartypeContext)_localctx).t =  new Type("float"); (_localctx.t).isVar = true;
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(227); match(T__15);
				setState(228); floatconst();
				setState(229); match(T__0);
				setState(230); floatconst();
				((BasicvartypeContext)_localctx).t =  new Type("float"); (_localctx.t).isVar = true;
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(233); match(T__15);
				setState(234); match(T__17);
				((BasicvartypeContext)_localctx).t =  new Type("int"); (_localctx.t).isVar = true;
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(236); match(T__15);
				setState(237); ((BasicvartypeContext)_localctx).setconst = setconst();
				((BasicvartypeContext)_localctx).t =  new Type("int"); (_localctx.t).isVar = true; ((BasicvartypeContext)_localctx).d =  ((BasicvartypeContext)_localctx).setconst.e;
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(240); match(T__15);
				setState(241); match(T__14);
				setState(242); match(T__22);
				setState(243); ((BasicvartypeContext)_localctx).setconst = setconst();
				((BasicvartypeContext)_localctx).t =  new Type("set"); (_localctx.t).isVar = true; ((BasicvartypeContext)_localctx).d =  ((BasicvartypeContext)_localctx).setconst.e;
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class VarpredparamtypeContext extends ParserRuleContext {
		public Type t;
		public VartypeContext vartype;
		public ArraytypeContext arraytype;
		public ArraytypeContext arraytype() {
			return getRuleContext(ArraytypeContext.class,0);
		}
		public VartypeContext vartype() {
			return getRuleContext(VartypeContext.class,0);
		}
		public VarpredparamtypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_varpredparamtype; }
	}

	public final VarpredparamtypeContext varpredparamtype() throws RecognitionException {
		VarpredparamtypeContext _localctx = new VarpredparamtypeContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_varpredparamtype);
		try {
			setState(263);
			switch ( getInterpreter().adaptivePredict(_input,15,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(248); ((VarpredparamtypeContext)_localctx).vartype = vartype();
				((VarpredparamtypeContext)_localctx).t =  ((VarpredparamtypeContext)_localctx).vartype.t;
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(251); match(T__15);
				setState(252); match(T__14);
				setState(253); match(T__22);
				setState(254); match(T__17);
				((VarpredparamtypeContext)_localctx).t =  new Type("set"); (_localctx.t).isVar = true;
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(256); ((VarpredparamtypeContext)_localctx).arraytype = arraytype();
				setState(257); match(T__15);
				setState(258); match(T__14);
				setState(259); match(T__22);
				setState(260); match(T__17);
				((VarpredparamtypeContext)_localctx).t =  new Type("set"); (_localctx.t).isVar = true; (_localctx.t).isArray=true; (_localctx.t).size = ((VarpredparamtypeContext)_localctx).arraytype.size;
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ArraytypeContext extends ParserRuleContext {
		public int size;
		public IntconstContext lb;
		public IntconstContext ub;
		public IntconstContext intconst(int i) {
			return getRuleContext(IntconstContext.class,i);
		}
		public List<IntconstContext> intconst() {
			return getRuleContexts(IntconstContext.class);
		}
		public ArraytypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_arraytype; }
	}

	public final ArraytypeContext arraytype() throws RecognitionException {
		ArraytypeContext _localctx = new ArraytypeContext(_ctx, getState());
		enterRule(_localctx, 30, RULE_arraytype);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(265); match(T__16);
			setState(266); match(T__20);
			setState(274);
			switch (_input.LA(1)) {
			case INT:
				{
				setState(267); ((ArraytypeContext)_localctx).lb = intconst();
				setState(268); match(T__0);
				setState(269); ((ArraytypeContext)_localctx).ub = intconst();
				((ArraytypeContext)_localctx).size =  ((ArraytypeContext)_localctx).ub.i; if(((ArraytypeContext)_localctx).lb.i!=1) throw new ParsingException("Ranges of array must start at 1");
				}
				break;
			case T__17:
				{
				setState(272); match(T__17);
				((ArraytypeContext)_localctx).size =  -1;
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(276); match(T__24);
			setState(277); match(T__22);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprContext extends ParserRuleContext {
		public Element e;
		public Token Boolconst;
		public Token Floatconst;
		public IntconstContext intconst;
		public SetconstContext setconst;
		public VarparidContext varparid;
		public ArrayexprContext arrayexpr;
		public AnnotationContext annotation;
		public StringconstantContext stringconstant;
		public VarparidContext varparid() {
			return getRuleContext(VarparidContext.class,0);
		}
		public AnnotationContext annotation() {
			return getRuleContext(AnnotationContext.class,0);
		}
		public SetconstContext setconst() {
			return getRuleContext(SetconstContext.class,0);
		}
		public TerminalNode Floatconst() { return getToken(FlatzincParser.Floatconst, 0); }
		public ArrayexprContext arrayexpr() {
			return getRuleContext(ArrayexprContext.class,0);
		}
		public IntconstContext intconst() {
			return getRuleContext(IntconstContext.class,0);
		}
		public StringconstantContext stringconstant() {
			return getRuleContext(StringconstantContext.class,0);
		}
		public TerminalNode Boolconst() { return getToken(FlatzincParser.Boolconst, 0); }
		public ExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expr; }
	}

	public final ExprContext expr() throws RecognitionException {
		ExprContext _localctx = new ExprContext(_ctx, getState());
		enterRule(_localctx, 32, RULE_expr);
		try {
			setState(307);
			switch ( getInterpreter().adaptivePredict(_input,17,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(279); ((ExprContext)_localctx).Boolconst = match(Boolconst);
				((ExprContext)_localctx).e =  new Element(); (_localctx.e).value = ((ExprContext)_localctx).Boolconst.getText().equals("true"); (_localctx.e).typ = new Type("bool");
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(281); ((ExprContext)_localctx).Floatconst = match(Floatconst);
				((ExprContext)_localctx).e =  new Element(); (_localctx.e).value = Float.parseFloat(((ExprContext)_localctx).Floatconst.getText()); (_localctx.e).typ = new Type("float");
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(283); ((ExprContext)_localctx).intconst = intconst();
				((ExprContext)_localctx).e =  new Element(); (_localctx.e).value = ((ExprContext)_localctx).intconst.i; (_localctx.e).typ = new Type("int");
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(286); ((ExprContext)_localctx).setconst = setconst();
				((ExprContext)_localctx).e =  ((ExprContext)_localctx).setconst.e; (_localctx.e).typ = new Type("set");
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(289); ((ExprContext)_localctx).varparid = varparid();
				((ExprContext)_localctx).e =  m.findId(((ExprContext)_localctx).varparid.text); 
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(292); ((ExprContext)_localctx).varparid = varparid();
				setState(293); match(T__20);
				setState(294); ((ExprContext)_localctx).intconst = intconst();
				setState(295); match(T__24);
				((ExprContext)_localctx).e =  ((ArrayOfElement)m.findId(((ExprContext)_localctx).varparid.text)).elements.get(((ExprContext)_localctx).intconst.i-1); 
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(298); ((ExprContext)_localctx).arrayexpr = arrayexpr();
				((ExprContext)_localctx).e =  ((ExprContext)_localctx).arrayexpr.a;
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(301); ((ExprContext)_localctx).annotation = annotation();
				((ExprContext)_localctx).e =  new Element(); (_localctx.e).value = ((ExprContext)_localctx).annotation.ann; (_localctx.e).typ = new Type("Annotation"); // TODO: Check this: Annotation and string expressions are only permitted in annotation arguments. 
					
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(304); ((ExprContext)_localctx).stringconstant = stringconstant();
				((ExprContext)_localctx).e =  new Element(); (_localctx.e).value = ((ExprContext)_localctx).stringconstant.str; (_localctx.e).typ = new Type("String");// TODO: Check this: Annotation and string expressions are only permitted in annotation arguments. 
					
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SetconstContext extends ParserRuleContext {
		public Element e;
		public Set<Integer> s;
		public IntconstContext lb;
		public IntconstContext ub;
		public IntconstContext f;
		public IntconstContext n;
		public IntconstContext intconst(int i) {
			return getRuleContext(IntconstContext.class,i);
		}
		public List<IntconstContext> intconst() {
			return getRuleContexts(IntconstContext.class);
		}
		public SetconstContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_setconst; }
	}

	public final SetconstContext setconst() throws RecognitionException {
		SetconstContext _localctx = new SetconstContext(_ctx, getState());
		enterRule(_localctx, 34, RULE_setconst);
		int _la;
		try {
			setState(331);
			switch (_input.LA(1)) {
			case INT:
				enterOuterAlt(_localctx, 1);
				{
				setState(309); ((SetconstContext)_localctx).lb = intconst();
				setState(310); match(T__0);
				setState(311); ((SetconstContext)_localctx).ub = intconst();
				((SetconstContext)_localctx).e =  new Element(); (_localctx.e).value = new DomainRange(((SetconstContext)_localctx).lb.i,((SetconstContext)_localctx).ub.i); 
				}
				break;
			case T__13:
				enterOuterAlt(_localctx, 2);
				{
				setState(314); match(T__13);
				((SetconstContext)_localctx).s =  new HashSet<Integer>();
				setState(327);
				_la = _input.LA(1);
				if (_la==INT) {
					{
					setState(316); ((SetconstContext)_localctx).f = intconst();
					 _localctx.s.add(((SetconstContext)_localctx).f.i); 
					setState(324);
					_errHandler.sync(this);
					_la = _input.LA(1);
					while (_la==T__21) {
						{
						{
						setState(318); match(T__21);
						setState(319); ((SetconstContext)_localctx).n = intconst();
						 _localctx.s.add(((SetconstContext)_localctx).n.i);
						}
						}
						setState(326);
						_errHandler.sync(this);
						_la = _input.LA(1);
					}
					}
				}

				setState(329); match(T__12);
				((SetconstContext)_localctx).e =  new Element(); (_localctx.e).value =m.createDomainSet(_localctx.s);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ArrayexprContext extends ParserRuleContext {
		public ArrayOfElement a;
		public ExprContext e;
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public ArrayexprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_arrayexpr; }
	}

	public final ArrayexprContext arrayexpr() throws RecognitionException {
		ArrayexprContext _localctx = new ArrayexprContext(_ctx, getState());
		enterRule(_localctx, 36, RULE_arrayexpr);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(333); match(T__20);
			((ArrayexprContext)_localctx).a =  new ArrayOfElement(); (_localctx.a).typ = new Type("null"); (_localctx.a).typ.isArray = true; _localctx.a.typ.size = 0;
			setState(346);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__20) | (1L << T__13) | (1L << Boolconst) | (1L << PREDANNID) | (1L << VARPARID) | (1L << Floatconst) | (1L << INT) | (1L << STRING))) != 0)) {
				{
				setState(335); ((ArrayexprContext)_localctx).e = expr();
				_localctx.a.elements.add(((ArrayexprContext)_localctx).e.e); _localctx.a.typ.size+=1; if(((ArrayexprContext)_localctx).e.e.typ.isVar)_localctx.a.typ.isVar = true; 
				setState(343);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==T__21) {
					{
					{
					setState(337); match(T__21);
					setState(338); ((ArrayexprContext)_localctx).e = expr();
					_localctx.a.elements.add(((ArrayexprContext)_localctx).e.e); _localctx.a.typ.size+=1; if(((ArrayexprContext)_localctx).e.e.typ.isVar)_localctx.a.typ.isVar = true; 
					}
					}
					setState(345);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				}
			}

			setState(348); match(T__24);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AnnotationsContext extends ParserRuleContext {
		public ArrayList<Annotation> anns;
		public AnnotationContext annotation;
		public AnnotationContext annotation(int i) {
			return getRuleContext(AnnotationContext.class,i);
		}
		public List<AnnotationContext> annotation() {
			return getRuleContexts(AnnotationContext.class);
		}
		public AnnotationsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_annotations; }
	}

	public final AnnotationsContext annotations() throws RecognitionException {
		AnnotationsContext _localctx = new AnnotationsContext(_ctx, getState());
		enterRule(_localctx, 38, RULE_annotations);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			((AnnotationsContext)_localctx).anns =  new ArrayList<Annotation>();
			setState(357);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__7) {
				{
				{
				setState(351); match(T__7);
				setState(352); ((AnnotationsContext)_localctx).annotation = annotation();
				_localctx.anns.add(((AnnotationsContext)_localctx).annotation.ann);
				}
				}
				setState(359);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AnnotationContext extends ParserRuleContext {
		public Annotation ann;
		public PredannidContext predannid;
		public ExprContext expr;
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public PredannidContext predannid() {
			return getRuleContext(PredannidContext.class,0);
		}
		public AnnotationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_annotation; }
	}

	public final AnnotationContext annotation() throws RecognitionException {
		AnnotationContext _localctx = new AnnotationContext(_ctx, getState());
		enterRule(_localctx, 40, RULE_annotation);
		int _la;
		try {
			setState(379);
			switch ( getInterpreter().adaptivePredict(_input,25,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(360); ((AnnotationContext)_localctx).predannid = predannid();
				((AnnotationContext)_localctx).ann =  new Annotation(((AnnotationContext)_localctx).predannid.text);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(363); ((AnnotationContext)_localctx).predannid = predannid();
				((AnnotationContext)_localctx).ann =  new Annotation(((AnnotationContext)_localctx).predannid.text);
				setState(365); match(T__19);
				setState(366); ((AnnotationContext)_localctx).expr = expr();
				(_localctx.ann).add(((AnnotationContext)_localctx).expr.e);
				setState(374);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==T__21) {
					{
					{
					setState(368); match(T__21);
					setState(369); ((AnnotationContext)_localctx).expr = expr();
					(_localctx.ann).add(((AnnotationContext)_localctx).expr.e);
					}
					}
					setState(376);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(377); match(T__8);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PredannidContext extends ParserRuleContext {
		public String text;
		public Token PREDANNID;
		public TerminalNode PREDANNID() { return getToken(FlatzincParser.PREDANNID, 0); }
		public PredannidContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_predannid; }
	}

	public final PredannidContext predannid() throws RecognitionException {
		PredannidContext _localctx = new PredannidContext(_ctx, getState());
		enterRule(_localctx, 42, RULE_predannid);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(381); ((PredannidContext)_localctx).PREDANNID = match(PREDANNID);
			((PredannidContext)_localctx).text = ((PredannidContext)_localctx).PREDANNID.getText();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class BoolconstContext extends ParserRuleContext {
		public TerminalNode Boolconst() { return getToken(FlatzincParser.Boolconst, 0); }
		public BoolconstContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_boolconst; }
	}

	public final BoolconstContext boolconst() throws RecognitionException {
		BoolconstContext _localctx = new BoolconstContext(_ctx, getState());
		enterRule(_localctx, 44, RULE_boolconst);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(384); match(Boolconst);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FloatconstContext extends ParserRuleContext {
		public TerminalNode Floatconst() { return getToken(FlatzincParser.Floatconst, 0); }
		public FloatconstContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_floatconst; }
	}

	public final FloatconstContext floatconst() throws RecognitionException {
		FloatconstContext _localctx = new FloatconstContext(_ctx, getState());
		enterRule(_localctx, 46, RULE_floatconst);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(386); match(Floatconst);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class IntconstContext extends ParserRuleContext {
		public int i;
		public Token INT;
		public TerminalNode INT() { return getToken(FlatzincParser.INT, 0); }
		public IntconstContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_intconst; }
	}

	public final IntconstContext intconst() throws RecognitionException {
		IntconstContext _localctx = new IntconstContext(_ctx, getState());
		enterRule(_localctx, 48, RULE_intconst);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(388); ((IntconstContext)_localctx).INT = match(INT);
			((IntconstContext)_localctx).i =  Integer.parseInt(((IntconstContext)_localctx).INT.getText());
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class StringconstantContext extends ParserRuleContext {
		public String str;
		public Token STRING;
		public TerminalNode STRING() { return getToken(FlatzincParser.STRING, 0); }
		public StringconstantContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_stringconstant; }
	}

	public final StringconstantContext stringconstant() throws RecognitionException {
		StringconstantContext _localctx = new StringconstantContext(_ctx, getState());
		enterRule(_localctx, 50, RULE_stringconstant);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(391); ((StringconstantContext)_localctx).STRING = match(STRING);
			((StringconstantContext)_localctx).str =  ((StringconstantContext)_localctx).STRING.getText().substring(1,((StringconstantContext)_localctx).STRING.getText().length()-1);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class VarparidContext extends ParserRuleContext {
		public String text;
		public Token VARPARID;
		public Token PREDANNID;
		public TerminalNode PREDANNID() { return getToken(FlatzincParser.PREDANNID, 0); }
		public TerminalNode VARPARID() { return getToken(FlatzincParser.VARPARID, 0); }
		public VarparidContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_varparid; }
	}

	public final VarparidContext varparid() throws RecognitionException {
		VarparidContext _localctx = new VarparidContext(_ctx, getState());
		enterRule(_localctx, 52, RULE_varparid);
		try {
			setState(398);
			switch (_input.LA(1)) {
			case VARPARID:
				enterOuterAlt(_localctx, 1);
				{
				setState(394); ((VarparidContext)_localctx).VARPARID = match(VARPARID);
				((VarparidContext)_localctx).text = ((VarparidContext)_localctx).VARPARID.getText();
				}
				break;
			case PREDANNID:
				enterOuterAlt(_localctx, 2);
				{
				setState(396); ((VarparidContext)_localctx).PREDANNID = match(PREDANNID);
				((VarparidContext)_localctx).text = ((VarparidContext)_localctx).PREDANNID.getText();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN =
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\3\"\u0193\4\2\t\2\4"+
		"\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t"+
		"\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31\t\31"+
		"\4\32\t\32\4\33\t\33\4\34\t\34\3\2\7\2:\n\2\f\2\16\2=\13\2\3\2\7\2@\n"+
		"\2\f\2\16\2C\13\2\3\2\7\2F\n\2\f\2\16\2I\13\2\3\2\7\2L\n\2\f\2\16\2O\13"+
		"\2\3\2\3\2\3\3\3\3\3\3\3\3\3\3\3\3\7\3Y\n\3\f\3\16\3\\\13\3\3\3\3\3\3"+
		"\3\3\4\3\4\3\4\3\4\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\6\3\6\3\6\3\6\3\6"+
		"\3\6\3\6\3\6\5\6u\n\6\3\6\3\6\3\6\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7"+
		"\7\7\u0083\n\7\f\7\16\7\u0086\13\7\3\7\3\7\3\7\3\7\3\7\3\b\3\b\3\b\3\b"+
		"\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\5\b\u00a1"+
		"\n\b\3\t\3\t\5\t\u00a5\n\t\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\5\n"+
		"\u00b1\n\n\3\13\3\13\3\13\3\13\3\13\3\13\3\13\5\13\u00ba\n\13\3\f\3\f"+
		"\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\5\f\u00c6\n\f\3\r\3\r\3\r\3\r\3\r\3\r"+
		"\3\r\3\r\3\r\3\r\3\r\3\r\3\r\5\r\u00d5\n\r\3\16\3\16\3\16\3\16\3\16\3"+
		"\16\3\16\5\16\u00de\n\16\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17"+
		"\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17"+
		"\3\17\3\17\5\17\u00f9\n\17\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20"+
		"\3\20\3\20\3\20\3\20\3\20\3\20\5\20\u010a\n\20\3\21\3\21\3\21\3\21\3\21"+
		"\3\21\3\21\3\21\3\21\5\21\u0115\n\21\3\21\3\21\3\21\3\22\3\22\3\22\3\22"+
		"\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22"+
		"\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\5\22\u0136\n\22\3\23"+
		"\3\23\3\23\3\23\3\23\3\23\3\23\3\23\3\23\3\23\3\23\3\23\3\23\7\23\u0145"+
		"\n\23\f\23\16\23\u0148\13\23\5\23\u014a\n\23\3\23\3\23\5\23\u014e\n\23"+
		"\3\24\3\24\3\24\3\24\3\24\3\24\3\24\3\24\7\24\u0158\n\24\f\24\16\24\u015b"+
		"\13\24\5\24\u015d\n\24\3\24\3\24\3\25\3\25\3\25\3\25\3\25\7\25\u0166\n"+
		"\25\f\25\16\25\u0169\13\25\3\26\3\26\3\26\3\26\3\26\3\26\3\26\3\26\3\26"+
		"\3\26\3\26\3\26\7\26\u0177\n\26\f\26\16\26\u017a\13\26\3\26\3\26\5\26"+
		"\u017e\n\26\3\27\3\27\3\27\3\30\3\30\3\31\3\31\3\32\3\32\3\32\3\33\3\33"+
		"\3\33\3\34\3\34\3\34\3\34\5\34\u0191\n\34\3\34\2\2\35\2\4\6\b\n\f\16\20"+
		"\22\24\26\30\32\34\36 \"$&(*,.\60\62\64\66\2\2\u01a3\2;\3\2\2\2\4R\3\2"+
		"\2\2\6`\3\2\2\2\bd\3\2\2\2\nl\3\2\2\2\fy\3\2\2\2\16\u00a0\3\2\2\2\20\u00a4"+
		"\3\2\2\2\22\u00b0\3\2\2\2\24\u00b9\3\2\2\2\26\u00c5\3\2\2\2\30\u00d4\3"+
		"\2\2\2\32\u00dd\3\2\2\2\34\u00f8\3\2\2\2\36\u0109\3\2\2\2 \u010b\3\2\2"+
		"\2\"\u0135\3\2\2\2$\u014d\3\2\2\2&\u014f\3\2\2\2(\u0160\3\2\2\2*\u017d"+
		"\3\2\2\2,\u017f\3\2\2\2.\u0182\3\2\2\2\60\u0184\3\2\2\2\62\u0186\3\2\2"+
		"\2\64\u0189\3\2\2\2\66\u0190\3\2\2\28:\5\4\3\298\3\2\2\2:=\3\2\2\2;9\3"+
		"\2\2\2;<\3\2\2\2<A\3\2\2\2=;\3\2\2\2>@\5\b\5\2?>\3\2\2\2@C\3\2\2\2A?\3"+
		"\2\2\2AB\3\2\2\2BG\3\2\2\2CA\3\2\2\2DF\5\n\6\2ED\3\2\2\2FI\3\2\2\2GE\3"+
		"\2\2\2GH\3\2\2\2HM\3\2\2\2IG\3\2\2\2JL\5\f\7\2KJ\3\2\2\2LO\3\2\2\2MK\3"+
		"\2\2\2MN\3\2\2\2NP\3\2\2\2OM\3\2\2\2PQ\5\16\b\2Q\3\3\2\2\2RS\7\21\2\2"+
		"ST\7\35\2\2TU\7\b\2\2UZ\5\6\4\2VW\7\6\2\2WY\5\6\4\2XV\3\2\2\2Y\\\3\2\2"+
		"\2ZX\3\2\2\2Z[\3\2\2\2[]\3\2\2\2\\Z\3\2\2\2]^\7\23\2\2^_\7\27\2\2_\5\3"+
		"\2\2\2`a\5\20\t\2ab\7\t\2\2bc\5,\27\2c\7\3\2\2\2de\5\24\13\2ef\7\t\2\2"+
		"fg\5\66\34\2gh\7\26\2\2hi\5\"\22\2ij\7\27\2\2jk\b\5\1\2k\t\3\2\2\2lm\5"+
		"\32\16\2mn\7\t\2\2no\5\66\34\2ot\5(\25\2pq\7\26\2\2qr\5\"\22\2rs\b\6\1"+
		"\2su\3\2\2\2tp\3\2\2\2tu\3\2\2\2uv\3\2\2\2vw\7\27\2\2wx\b\6\1\2x\13\3"+
		"\2\2\2yz\7\25\2\2z{\5,\27\2{|\7\b\2\2|}\5\"\22\2}\u0084\b\7\1\2~\177\7"+
		"\6\2\2\177\u0080\5\"\22\2\u0080\u0081\b\7\1\2\u0081\u0083\3\2\2\2\u0082"+
		"~\3\2\2\2\u0083\u0086\3\2\2\2\u0084\u0082\3\2\2\2\u0084\u0085\3\2\2\2"+
		"\u0085\u0087\3\2\2\2\u0086\u0084\3\2\2\2\u0087\u0088\7\23\2\2\u0088\u0089"+
		"\5(\25\2\u0089\u008a\b\7\1\2\u008a\u008b\7\27\2\2\u008b\r\3\2\2\2\u008c"+
		"\u008d\7\31\2\2\u008d\u008e\5(\25\2\u008e\u008f\7\32\2\2\u008f\u0090\7"+
		"\27\2\2\u0090\u0091\b\b\1\2\u0091\u00a1\3\2\2\2\u0092\u0093\7\31\2\2\u0093"+
		"\u0094\5(\25\2\u0094\u0095\7\4\2\2\u0095\u0096\5\"\22\2\u0096\u0097\7"+
		"\27\2\2\u0097\u0098\b\b\1\2\u0098\u00a1\3\2\2\2\u0099\u009a\7\31\2\2\u009a"+
		"\u009b\5(\25\2\u009b\u009c\7\30\2\2\u009c\u009d\5\"\22\2\u009d\u009e\7"+
		"\27\2\2\u009e\u009f\b\b\1\2\u009f\u00a1\3\2\2\2\u00a0\u008c\3\2\2\2\u00a0"+
		"\u0092\3\2\2\2\u00a0\u0099\3\2\2\2\u00a1\17\3\2\2\2\u00a2\u00a5\5\26\f"+
		"\2\u00a3\u00a5\5\36\20\2\u00a4\u00a2\3\2\2\2\u00a4\u00a3\3\2\2\2\u00a5"+
		"\21\3\2\2\2\u00a6\u00a7\7\22\2\2\u00a7\u00b1\b\n\1\2\u00a8\u00a9\7\20"+
		"\2\2\u00a9\u00b1\b\n\1\2\u00aa\u00ab\7\n\2\2\u00ab\u00b1\b\n\1\2\u00ac"+
		"\u00ad\7\r\2\2\u00ad\u00ae\7\5\2\2\u00ae\u00af\7\n\2\2\u00af\u00b1\b\n"+
		"\1\2\u00b0\u00a6\3\2\2\2\u00b0\u00a8\3\2\2\2\u00b0\u00aa\3\2\2\2\u00b0"+
		"\u00ac\3\2\2\2\u00b1\23\3\2\2\2\u00b2\u00b3\5\22\n\2\u00b3\u00b4\b\13"+
		"\1\2\u00b4\u00ba\3\2\2\2\u00b5\u00b6\5 \21\2\u00b6\u00b7\5\22\n\2\u00b7"+
		"\u00b8\b\13\1\2\u00b8\u00ba\3\2\2\2\u00b9\u00b2\3\2\2\2\u00b9\u00b5\3"+
		"\2\2\2\u00ba\25\3\2\2\2\u00bb\u00bc\5\24\13\2\u00bc\u00bd\b\f\1\2\u00bd"+
		"\u00c6\3\2\2\2\u00be\u00bf\5\30\r\2\u00bf\u00c0\b\f\1\2\u00c0\u00c6\3"+
		"\2\2\2\u00c1\u00c2\5 \21\2\u00c2\u00c3\5\30\r\2\u00c3\u00c4\b\f\1\2\u00c4"+
		"\u00c6\3\2\2\2\u00c5\u00bb\3\2\2\2\u00c5\u00be\3\2\2\2\u00c5\u00c1\3\2"+
		"\2\2\u00c6\27\3\2\2\2\u00c7\u00c8\5\60\31\2\u00c8\u00c9\7\33\2\2\u00c9"+
		"\u00ca\5\60\31\2\u00ca\u00cb\b\r\1\2\u00cb\u00d5\3\2\2\2\u00cc\u00cd\5"+
		"$\23\2\u00cd\u00ce\b\r\1\2\u00ce\u00d5\3\2\2\2\u00cf\u00d0\7\r\2\2\u00d0"+
		"\u00d1\7\5\2\2\u00d1\u00d2\5$\23\2\u00d2\u00d3\b\r\1\2\u00d3\u00d5\3\2"+
		"\2\2\u00d4\u00c7\3\2\2\2\u00d4\u00cc\3\2\2\2\u00d4\u00cf\3\2\2\2\u00d5"+
		"\31\3\2\2\2\u00d6\u00d7\5\34\17\2\u00d7\u00d8\b\16\1\2\u00d8\u00de\3\2"+
		"\2\2\u00d9\u00da\5 \21\2\u00da\u00db\5\34\17\2\u00db\u00dc\b\16\1\2\u00dc"+
		"\u00de\3\2\2\2\u00dd\u00d6\3\2\2\2\u00dd\u00d9\3\2\2\2\u00de\33\3\2\2"+
		"\2\u00df\u00e0\7\f\2\2\u00e0\u00e1\7\22\2\2\u00e1\u00f9\b\17\1\2\u00e2"+
		"\u00e3\7\f\2\2\u00e3\u00e4\7\20\2\2\u00e4\u00f9\b\17\1\2\u00e5\u00e6\7"+
		"\f\2\2\u00e6\u00e7\5\60\31\2\u00e7\u00e8\7\33\2\2\u00e8\u00e9\5\60\31"+
		"\2\u00e9\u00ea\b\17\1\2\u00ea\u00f9\3\2\2\2\u00eb\u00ec\7\f\2\2\u00ec"+
		"\u00ed\7\n\2\2\u00ed\u00f9\b\17\1\2\u00ee\u00ef\7\f\2\2\u00ef\u00f0\5"+
		"$\23\2\u00f0\u00f1\b\17\1\2\u00f1\u00f9\3\2\2\2\u00f2\u00f3\7\f\2\2\u00f3"+
		"\u00f4\7\r\2\2\u00f4\u00f5\7\5\2\2\u00f5\u00f6\5$\23\2\u00f6\u00f7\b\17"+
		"\1\2\u00f7\u00f9\3\2\2\2\u00f8\u00df\3\2\2\2\u00f8\u00e2\3\2\2\2\u00f8"+
		"\u00e5\3\2\2\2\u00f8\u00eb\3\2\2\2\u00f8\u00ee\3\2\2\2\u00f8\u00f2\3\2"+
		"\2\2\u00f9\35\3\2\2\2\u00fa\u00fb\5\32\16\2\u00fb\u00fc\b\20\1\2\u00fc"+
		"\u010a\3\2\2\2\u00fd\u00fe\7\f\2\2\u00fe\u00ff\7\r\2\2\u00ff\u0100\7\5"+
		"\2\2\u0100\u0101\7\n\2\2\u0101\u010a\b\20\1\2\u0102\u0103\5 \21\2\u0103"+
		"\u0104\7\f\2\2\u0104\u0105\7\r\2\2\u0105\u0106\7\5\2\2\u0106\u0107\7\n"+
		"\2\2\u0107\u0108\b\20\1\2\u0108\u010a\3\2\2\2\u0109\u00fa\3\2\2\2\u0109"+
		"\u00fd\3\2\2\2\u0109\u0102\3\2\2\2\u010a\37\3\2\2\2\u010b\u010c\7\13\2"+
		"\2\u010c\u0114\7\7\2\2\u010d\u010e\5\62\32\2\u010e\u010f\7\33\2\2\u010f"+
		"\u0110\5\62\32\2\u0110\u0111\b\21\1\2\u0111\u0115\3\2\2\2\u0112\u0113"+
		"\7\n\2\2\u0113\u0115\b\21\1\2\u0114\u010d\3\2\2\2\u0114\u0112\3\2\2\2"+
		"\u0115\u0116\3\2\2\2\u0116\u0117\7\3\2\2\u0117\u0118\7\5\2\2\u0118!\3"+
		"\2\2\2\u0119\u011a\7\34\2\2\u011a\u0136\b\22\1\2\u011b\u011c\7\37\2\2"+
		"\u011c\u0136\b\22\1\2\u011d\u011e\5\62\32\2\u011e\u011f\b\22\1\2\u011f"+
		"\u0136\3\2\2\2\u0120\u0121\5$\23\2\u0121\u0122\b\22\1\2\u0122\u0136\3"+
		"\2\2\2\u0123\u0124\5\66\34\2\u0124\u0125\b\22\1\2\u0125\u0136\3\2\2\2"+
		"\u0126\u0127\5\66\34\2\u0127\u0128\7\7\2\2\u0128\u0129\5\62\32\2\u0129"+
		"\u012a\7\3\2\2\u012a\u012b\b\22\1\2\u012b\u0136\3\2\2\2\u012c\u012d\5"+
		"&\24\2\u012d\u012e\b\22\1\2\u012e\u0136\3\2\2\2\u012f\u0130\5*\26\2\u0130"+
		"\u0131\b\22\1\2\u0131\u0136\3\2\2\2\u0132\u0133\5\64\33\2\u0133\u0134"+
		"\b\22\1\2\u0134\u0136\3\2\2\2\u0135\u0119\3\2\2\2\u0135\u011b\3\2\2\2"+
		"\u0135\u011d\3\2\2\2\u0135\u0120\3\2\2\2\u0135\u0123\3\2\2\2\u0135\u0126"+
		"\3\2\2\2\u0135\u012c\3\2\2\2\u0135\u012f\3\2\2\2\u0135\u0132\3\2\2\2\u0136"+
		"#\3\2\2\2\u0137\u0138\5\62\32\2\u0138\u0139\7\33\2\2\u0139\u013a\5\62"+
		"\32\2\u013a\u013b\b\23\1\2\u013b\u014e\3\2\2\2\u013c\u013d\7\16\2\2\u013d"+
		"\u0149\b\23\1\2\u013e\u013f\5\62\32\2\u013f\u0146\b\23\1\2\u0140\u0141"+
		"\7\6\2\2\u0141\u0142\5\62\32\2\u0142\u0143\b\23\1\2\u0143\u0145\3\2\2"+
		"\2\u0144\u0140\3\2\2\2\u0145\u0148\3\2\2\2\u0146\u0144\3\2\2\2\u0146\u0147"+
		"\3\2\2\2\u0147\u014a\3\2\2\2\u0148\u0146\3\2\2\2\u0149\u013e\3\2\2\2\u0149"+
		"\u014a\3\2\2\2\u014a\u014b\3\2\2\2\u014b\u014c\7\17\2\2\u014c\u014e\b"+
		"\23\1\2\u014d\u0137\3\2\2\2\u014d\u013c\3\2\2\2\u014e%\3\2\2\2\u014f\u0150"+
		"\7\7\2\2\u0150\u015c\b\24\1\2\u0151\u0152\5\"\22\2\u0152\u0159\b\24\1"+
		"\2\u0153\u0154\7\6\2\2\u0154\u0155\5\"\22\2\u0155\u0156\b\24\1\2\u0156"+
		"\u0158\3\2\2\2\u0157\u0153\3\2\2\2\u0158\u015b\3\2\2\2\u0159\u0157\3\2"+
		"\2\2\u0159\u015a\3\2\2\2\u015a\u015d\3\2\2\2\u015b\u0159\3\2\2\2\u015c"+
		"\u0151\3\2\2\2\u015c\u015d\3\2\2\2\u015d\u015e\3\2\2\2\u015e\u015f\7\3"+
		"\2\2\u015f\'\3\2\2\2\u0160\u0167\b\25\1\2\u0161\u0162\7\24\2\2\u0162\u0163"+
		"\5*\26\2\u0163\u0164\b\25\1\2\u0164\u0166\3\2\2\2\u0165\u0161\3\2\2\2"+
		"\u0166\u0169\3\2\2\2\u0167\u0165\3\2\2\2\u0167\u0168\3\2\2\2\u0168)\3"+
		"\2\2\2\u0169\u0167\3\2\2\2\u016a\u016b\5,\27\2\u016b\u016c\b\26\1\2\u016c"+
		"\u017e\3\2\2\2\u016d\u016e\5,\27\2\u016e\u016f\b\26\1\2\u016f\u0170\7"+
		"\b\2\2\u0170\u0171\5\"\22\2\u0171\u0178\b\26\1\2\u0172\u0173\7\6\2\2\u0173"+
		"\u0174\5\"\22\2\u0174\u0175\b\26\1\2\u0175\u0177\3\2\2\2\u0176\u0172\3"+
		"\2\2\2\u0177\u017a\3\2\2\2\u0178\u0176\3\2\2\2\u0178\u0179\3\2\2\2\u0179"+
		"\u017b\3\2\2\2\u017a\u0178\3\2\2\2\u017b\u017c\7\23\2\2\u017c\u017e\3"+
		"\2\2\2\u017d\u016a\3\2\2\2\u017d\u016d\3\2\2\2\u017e+\3\2\2\2\u017f\u0180"+
		"\7\35\2\2\u0180\u0181\b\27\1\2\u0181-\3\2\2\2\u0182\u0183\7\34\2\2\u0183"+
		"/\3\2\2\2\u0184\u0185\7\37\2\2\u0185\61\3\2\2\2\u0186\u0187\7 \2\2\u0187"+
		"\u0188\b\32\1\2\u0188\63\3\2\2\2\u0189\u018a\7!\2\2\u018a\u018b\b\33\1"+
		"\2\u018b\65\3\2\2\2\u018c\u018d\7\36\2\2\u018d\u0191\b\34\1\2\u018e\u018f"+
		"\7\35\2\2\u018f\u0191\b\34\1\2\u0190\u018c\3\2\2\2\u0190\u018e\3\2\2\2"+
		"\u0191\67\3\2\2\2\35;AGMZt\u0084\u00a0\u00a4\u00b0\u00b9\u00c5\u00d4\u00dd"+
		"\u00f8\u0109\u0114\u0135\u0146\u0149\u014d\u0159\u015c\u0167\u0178\u017d"+
		"\u0190";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}