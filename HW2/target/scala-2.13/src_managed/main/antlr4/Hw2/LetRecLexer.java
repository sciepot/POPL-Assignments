// Generated from D:\Mystuff\2022 FALL\Principles of Programming Languages\Homeworks\HW2\src\main\antlr4\LetRecLexer.g4 by ANTLR 4.8
package Hw2;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class LetRecLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.8", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		WS=1, INT=2, LET=3, IN=4, PLUS=5, MINUS=6, LETREC=7, ISZERO=8, IF=9, THEN=10, 
		ELSE=11, EQ=12, LPAREN=13, RPAREN=14, PROC=15, ID=16;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	private static String[] makeRuleNames() {
		return new String[] {
			"WS", "INT", "LET", "IN", "PLUS", "MINUS", "LETREC", "ISZERO", "IF", 
			"THEN", "ELSE", "EQ", "LPAREN", "RPAREN", "PROC", "ID", "DIGIT", "ALPHA"
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


	public LetRecLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "LetRecLexer.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getChannelNames() { return channelNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\22m\b\1\4\2\t\2\4"+
		"\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t"+
		"\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\3\2\6\2)\n\2\r\2\16\2*\3\2\3\2\3\3\6\3\60\n\3\r\3\16\3\61\3"+
		"\4\3\4\3\4\3\4\3\5\3\5\3\5\3\6\3\6\3\7\3\7\3\b\3\b\3\b\3\b\3\b\3\b\3\b"+
		"\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\n\3\n\3\n\3\13\3\13\3\13\3\13\3\13\3\f"+
		"\3\f\3\f\3\f\3\f\3\r\3\r\3\16\3\16\3\17\3\17\3\20\3\20\3\20\3\20\3\20"+
		"\3\21\6\21f\n\21\r\21\16\21g\3\22\3\22\3\23\3\23\2\2\24\3\3\5\4\7\5\t"+
		"\6\13\7\r\b\17\t\21\n\23\13\25\f\27\r\31\16\33\17\35\20\37\21!\22#\2%"+
		"\2\3\2\5\4\2\13\f\"\"\3\2\62;\4\2C\\c|\2m\2\3\3\2\2\2\2\5\3\2\2\2\2\7"+
		"\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3\2\2"+
		"\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2\2\31\3\2\2\2\2\33\3\2\2\2\2"+
		"\35\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\3(\3\2\2\2\5/\3\2\2\2\7\63\3\2\2\2"+
		"\t\67\3\2\2\2\13:\3\2\2\2\r<\3\2\2\2\17>\3\2\2\2\21E\3\2\2\2\23L\3\2\2"+
		"\2\25O\3\2\2\2\27T\3\2\2\2\31Y\3\2\2\2\33[\3\2\2\2\35]\3\2\2\2\37_\3\2"+
		"\2\2!e\3\2\2\2#i\3\2\2\2%k\3\2\2\2\')\t\2\2\2(\'\3\2\2\2)*\3\2\2\2*(\3"+
		"\2\2\2*+\3\2\2\2+,\3\2\2\2,-\b\2\2\2-\4\3\2\2\2.\60\5#\22\2/.\3\2\2\2"+
		"\60\61\3\2\2\2\61/\3\2\2\2\61\62\3\2\2\2\62\6\3\2\2\2\63\64\7n\2\2\64"+
		"\65\7g\2\2\65\66\7v\2\2\66\b\3\2\2\2\678\7k\2\289\7p\2\29\n\3\2\2\2:;"+
		"\7-\2\2;\f\3\2\2\2<=\7/\2\2=\16\3\2\2\2>?\7n\2\2?@\7g\2\2@A\7v\2\2AB\7"+
		"t\2\2BC\7g\2\2CD\7e\2\2D\20\3\2\2\2EF\7k\2\2FG\7u\2\2GH\7|\2\2HI\7g\2"+
		"\2IJ\7t\2\2JK\7q\2\2K\22\3\2\2\2LM\7k\2\2MN\7h\2\2N\24\3\2\2\2OP\7v\2"+
		"\2PQ\7j\2\2QR\7g\2\2RS\7p\2\2S\26\3\2\2\2TU\7g\2\2UV\7n\2\2VW\7u\2\2W"+
		"X\7g\2\2X\30\3\2\2\2YZ\7?\2\2Z\32\3\2\2\2[\\\7*\2\2\\\34\3\2\2\2]^\7+"+
		"\2\2^\36\3\2\2\2_`\7r\2\2`a\7t\2\2ab\7q\2\2bc\7e\2\2c \3\2\2\2df\5%\23"+
		"\2ed\3\2\2\2fg\3\2\2\2ge\3\2\2\2gh\3\2\2\2h\"\3\2\2\2ij\t\3\2\2j$\3\2"+
		"\2\2kl\t\4\2\2l&\3\2\2\2\6\2*\61g\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}