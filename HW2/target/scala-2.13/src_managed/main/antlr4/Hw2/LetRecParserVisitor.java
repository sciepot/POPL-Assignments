// Generated from D:\Mystuff\2022 FALL\Principles of Programming Languages\Homeworks\HW2\src\main\antlr4\LetRecParser.g4 by ANTLR 4.8
package Hw2;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link LetRecParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface LetRecParserVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link LetRecParser#program}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProgram(LetRecParser.ProgramContext ctx);
	/**
	 * Visit a parse tree produced by {@link LetRecParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExpr(LetRecParser.ExprContext ctx);
	/**
	 * Visit a parse tree produced by {@link LetRecParser#integer}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInteger(LetRecParser.IntegerContext ctx);
	/**
	 * Visit a parse tree produced by {@link LetRecParser#id}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitId(LetRecParser.IdContext ctx);
}