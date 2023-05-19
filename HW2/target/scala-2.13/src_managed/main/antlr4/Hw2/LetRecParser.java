// Generated from D:\Mystuff\2022 FALL\Principles of Programming Languages\Homeworks\HW2\src\main\antlr4\LetRecParser.g4 by ANTLR 4.8
package Hw2;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class LetRecParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.8", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		WS=1, INT=2, LET=3, IN=4, PLUS=5, MINUS=6, LETREC=7, ISZERO=8, IF=9, THEN=10, 
		ELSE=11, EQ=12, LPAREN=13, RPAREN=14, PROC=15, ID=16;
	public static final int
		RULE_program = 0, RULE_expr = 1, RULE_integer = 2, RULE_id = 3;
	private static String[] makeRuleNames() {
		return new String[] {
			"program", "expr", "integer", "id"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, null, null, "'let'", "'in'", "'+'", "'-'", "'letrec'", "'iszero'", 
			"'if'", "'then'", "'else'", "'='", "'('", "')'", "'proc'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, "WS", "INT", "LET", "IN", "PLUS", "MINUS", "LETREC", "ISZERO", 
			"IF", "THEN", "ELSE", "EQ", "LPAREN", "RPAREN", "PROC", "ID"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "LetRecParser.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public LetRecParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	public static class ProgramContext extends ParserRuleContext {
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode EOF() { return getToken(LetRecParser.EOF, 0); }
		public ProgramContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_program; }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof LetRecParserVisitor ) return ((LetRecParserVisitor<? extends T>)visitor).visitProgram(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ProgramContext program() throws RecognitionException {
		ProgramContext _localctx = new ProgramContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_program);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(8);
			expr(0);
			setState(9);
			match(EOF);
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
		public IntegerContext integer() {
			return getRuleContext(IntegerContext.class,0);
		}
		public List<IdContext> id() {
			return getRuleContexts(IdContext.class);
		}
		public IdContext id(int i) {
			return getRuleContext(IdContext.class,i);
		}
		public TerminalNode ISZERO() { return getToken(LetRecParser.ISZERO, 0); }
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public TerminalNode IF() { return getToken(LetRecParser.IF, 0); }
		public TerminalNode THEN() { return getToken(LetRecParser.THEN, 0); }
		public TerminalNode ELSE() { return getToken(LetRecParser.ELSE, 0); }
		public TerminalNode LET() { return getToken(LetRecParser.LET, 0); }
		public TerminalNode EQ() { return getToken(LetRecParser.EQ, 0); }
		public TerminalNode IN() { return getToken(LetRecParser.IN, 0); }
		public TerminalNode LETREC() { return getToken(LetRecParser.LETREC, 0); }
		public TerminalNode LPAREN() { return getToken(LetRecParser.LPAREN, 0); }
		public TerminalNode RPAREN() { return getToken(LetRecParser.RPAREN, 0); }
		public TerminalNode PROC() { return getToken(LetRecParser.PROC, 0); }
		public TerminalNode PLUS() { return getToken(LetRecParser.PLUS, 0); }
		public TerminalNode MINUS() { return getToken(LetRecParser.MINUS, 0); }
		public ExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expr; }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof LetRecParserVisitor ) return ((LetRecParserVisitor<? extends T>)visitor).visitExpr(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ExprContext expr() throws RecognitionException {
		return expr(0);
	}

	private ExprContext expr(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		ExprContext _localctx = new ExprContext(_ctx, _parentState);
		ExprContext _prevctx = _localctx;
		int _startState = 2;
		enterRecursionRule(_localctx, 2, RULE_expr, _p);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(48);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case INT:
				{
				setState(12);
				integer();
				}
				break;
			case ID:
				{
				setState(13);
				id();
				}
				break;
			case ISZERO:
				{
				setState(14);
				match(ISZERO);
				setState(15);
				expr(7);
				}
				break;
			case IF:
				{
				setState(16);
				match(IF);
				setState(17);
				expr(0);
				setState(18);
				match(THEN);
				setState(19);
				expr(0);
				setState(20);
				match(ELSE);
				setState(21);
				expr(6);
				}
				break;
			case LET:
				{
				setState(23);
				match(LET);
				setState(24);
				id();
				setState(25);
				match(EQ);
				setState(26);
				expr(0);
				setState(27);
				match(IN);
				setState(28);
				expr(5);
				}
				break;
			case LETREC:
				{
				setState(30);
				match(LETREC);
				setState(31);
				id();
				setState(32);
				match(LPAREN);
				setState(33);
				id();
				setState(34);
				match(RPAREN);
				setState(35);
				match(EQ);
				setState(36);
				expr(0);
				setState(37);
				match(IN);
				setState(38);
				expr(4);
				}
				break;
			case PROC:
				{
				setState(40);
				match(PROC);
				setState(41);
				id();
				setState(42);
				expr(3);
				}
				break;
			case LPAREN:
				{
				setState(44);
				match(LPAREN);
				setState(45);
				expr(0);
				setState(46);
				match(RPAREN);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(57);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,2,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					setState(55);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,1,_ctx) ) {
					case 1:
						{
						_localctx = new ExprContext(_parentctx, _parentState);
						pushNewRecursionContext(_localctx, _startState, RULE_expr);
						setState(50);
						if (!(precpred(_ctx, 8))) throw new FailedPredicateException(this, "precpred(_ctx, 8)");
						setState(51);
						_la = _input.LA(1);
						if ( !(_la==PLUS || _la==MINUS) ) {
						_errHandler.recoverInline(this);
						}
						else {
							if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
							_errHandler.reportMatch(this);
							consume();
						}
						setState(52);
						expr(9);
						}
						break;
					case 2:
						{
						_localctx = new ExprContext(_parentctx, _parentState);
						pushNewRecursionContext(_localctx, _startState, RULE_expr);
						setState(53);
						if (!(precpred(_ctx, 1))) throw new FailedPredicateException(this, "precpred(_ctx, 1)");
						setState(54);
						expr(2);
						}
						break;
					}
					} 
				}
				setState(59);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,2,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class IntegerContext extends ParserRuleContext {
		public TerminalNode INT() { return getToken(LetRecParser.INT, 0); }
		public IntegerContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_integer; }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof LetRecParserVisitor ) return ((LetRecParserVisitor<? extends T>)visitor).visitInteger(this);
			else return visitor.visitChildren(this);
		}
	}

	public final IntegerContext integer() throws RecognitionException {
		IntegerContext _localctx = new IntegerContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_integer);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(60);
			match(INT);
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

	public static class IdContext extends ParserRuleContext {
		public TerminalNode ID() { return getToken(LetRecParser.ID, 0); }
		public IdContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_id; }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof LetRecParserVisitor ) return ((LetRecParserVisitor<? extends T>)visitor).visitId(this);
			else return visitor.visitChildren(this);
		}
	}

	public final IdContext id() throws RecognitionException {
		IdContext _localctx = new IdContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_id);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(62);
			match(ID);
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

	public boolean sempred(RuleContext _localctx, int ruleIndex, int predIndex) {
		switch (ruleIndex) {
		case 1:
			return expr_sempred((ExprContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean expr_sempred(ExprContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0:
			return precpred(_ctx, 8);
		case 1:
			return precpred(_ctx, 1);
		}
		return true;
	}

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\22C\4\2\t\2\4\3\t"+
		"\3\4\4\t\4\4\5\t\5\3\2\3\2\3\2\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3"+
		"\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3"+
		"\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\5\3\63\n\3\3\3\3\3\3\3\3\3\3\3"+
		"\7\3:\n\3\f\3\16\3=\13\3\3\4\3\4\3\5\3\5\3\5\2\3\4\6\2\4\6\b\2\3\3\2\7"+
		"\b\2G\2\n\3\2\2\2\4\62\3\2\2\2\6>\3\2\2\2\b@\3\2\2\2\n\13\5\4\3\2\13\f"+
		"\7\2\2\3\f\3\3\2\2\2\r\16\b\3\1\2\16\63\5\6\4\2\17\63\5\b\5\2\20\21\7"+
		"\n\2\2\21\63\5\4\3\t\22\23\7\13\2\2\23\24\5\4\3\2\24\25\7\f\2\2\25\26"+
		"\5\4\3\2\26\27\7\r\2\2\27\30\5\4\3\b\30\63\3\2\2\2\31\32\7\5\2\2\32\33"+
		"\5\b\5\2\33\34\7\16\2\2\34\35\5\4\3\2\35\36\7\6\2\2\36\37\5\4\3\7\37\63"+
		"\3\2\2\2 !\7\t\2\2!\"\5\b\5\2\"#\7\17\2\2#$\5\b\5\2$%\7\20\2\2%&\7\16"+
		"\2\2&\'\5\4\3\2\'(\7\6\2\2()\5\4\3\6)\63\3\2\2\2*+\7\21\2\2+,\5\b\5\2"+
		",-\5\4\3\5-\63\3\2\2\2./\7\17\2\2/\60\5\4\3\2\60\61\7\20\2\2\61\63\3\2"+
		"\2\2\62\r\3\2\2\2\62\17\3\2\2\2\62\20\3\2\2\2\62\22\3\2\2\2\62\31\3\2"+
		"\2\2\62 \3\2\2\2\62*\3\2\2\2\62.\3\2\2\2\63;\3\2\2\2\64\65\f\n\2\2\65"+
		"\66\t\2\2\2\66:\5\4\3\13\678\f\3\2\28:\5\4\3\49\64\3\2\2\29\67\3\2\2\2"+
		":=\3\2\2\2;9\3\2\2\2;<\3\2\2\2<\5\3\2\2\2=;\3\2\2\2>?\7\4\2\2?\7\3\2\2"+
		"\2@A\7\22\2\2A\t\3\2\2\2\5\629;";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}