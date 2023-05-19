// Generated from D:\Mystuff\2022 FALL\Principles of Programming Languages\Homeworks\HW2\src\main\antlr4\IntParserAntlr4.g4 by ANTLR 4.8
package Hw2;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link IntParserAntlr4}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface IntParserAntlr4Visitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link IntParserAntlr4#program}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProgram(IntParserAntlr4.ProgramContext ctx);
	/**
	 * Visit a parse tree produced by {@link IntParserAntlr4#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExpr(IntParserAntlr4.ExprContext ctx);
	/**
	 * Visit a parse tree produced by {@link IntParserAntlr4#integer}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInteger(IntParserAntlr4.IntegerContext ctx);
	/**
	 * Visit a parse tree produced by {@link IntParserAntlr4#id}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitId(IntParserAntlr4.IdContext ctx);
}