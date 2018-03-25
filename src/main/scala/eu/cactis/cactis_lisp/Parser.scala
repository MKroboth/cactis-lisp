package eu.cactis.cactis_lisp
import eu.cactis.cactis_lisp.ast._

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Reader}

sealed trait Token
case class IdentifierToken(string: String) extends Token
case class NumberToken(bigInt: BigInt, radix: Int=10) extends Token
case class DecimalToken(bigDecimal: BigDecimal) extends Token
case class CommentToken(string: String) extends Token
case class StringToken(string: String) extends Token
case object LBracket extends Token
case object RBracket extends Token
case object LParen extends Token
case object RParen extends Token
case object Quote extends Token
case object Backquote extends Token
case object Unquote extends Token
case object At extends Token
case object Dot extends Token
case object NilToken extends Token


trait CactisLispCompilationError
case class CactisLispLexerError(msg: String) extends CactisLispCompilationError
case class CactisLispParserError(msg: String) extends CactisLispCompilationError

object Lexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f]+".r

  def comment_token: Parser[CommentToken] = """;[^\n]*\n""".r ^^ { x => CommentToken(x) }
  def string_token: Parser[StringToken] = """"[^"]+"""".r ^^ { x => StringToken(x.drop(1).dropRight(1)) }
  def number_token: Parser[NumberToken] = """(\+|\-)?(?:0(x|X)[0-9A-Fa-f]+)|(?:0|[1-9][0-9]*)""".r ^^ {
    case x if x.contains("x") || x.contains("X") =>
      val sign = x.charAt(0) match {
        case y if y != '+' && y != '-' => '+'
        case y => y
      }

      NumberToken(BigInt(sign + x.dropWhile(_ == sign).substring(2), 16), 16)
    case x => NumberToken(BigInt(x), 10)
  }
  def decimal_token: Parser[DecimalToken] = "\\d+\\.\\d+".r ^^ { x => DecimalToken(BigDecimal(x)) }
  def identifier_token: Parser[IdentifierToken] = """[^0-9\(\)\[\]@'\s`.]+[^\(\)\[\]@'\s`.]*""".r ^^ { x => IdentifierToken(x) }

  def lparen: Parser[LParen.type] = "\\(".r ^^ { _ => LParen }
  def rparen: Parser[RParen.type] = "\\)".r ^^ { _ => RParen }
  def lbracket: Parser[LBracket.type] = "\\[".r ^^ { _ => LBracket }
  def rbracket: Parser[RBracket.type] = "\\]".r ^^ { _ => RBracket }
  def quote: Parser[Quote.type] = "'".r ^^ { _ => Quote }
  def backqoute: Parser[Backquote.type] = "`".r ^^ { _ => Backquote }
  def unquote: Parser[Unquote.type] = ",".r ^^ { _ => Unquote }
  def at: Parser[At.type] = "@".r ^^ { _ => At }
  def dot: Parser[Dot.type ] = "\\.".r ^^ { _ => Dot }

  def nil: Parser[NilToken.type ] = lparen ~ rparen ^^ { _ => NilToken }

  def token: Parser[Token] = comment_token | string_token | decimal_token | number_token | identifier_token | nil | lparen | rparen | lbracket | rbracket | quote | backqoute | unquote | at | dot

  def tokens: Parser[List[Token]] = phrase(rep1(token))

  def apply(code: String): Either[CactisLispLexerError, List[Token]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(CactisLispLexerError(msg))
      case Success(result, next) => Right(result)
    }
  }
}

class CactisLispTokenReader(tokens: Seq[Token]) extends Reader[Token] {
  override def first: Token = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[Token] = new CactisLispTokenReader(tokens.tail)
}





object Parser extends Parsers {
  override type Elem = Token

  def symbol_atom: Parser[SymbolAtom] = {
    accept("symbol", { case sym @ IdentifierToken(value) if !value.startsWith(":") || value.matches("::.*") => SymbolAtom(Symbol(value.trim))})
  }

  def keyword_atom: Parser[Keyword] = {
    accept("keyword", { case sym @ IdentifierToken(value) if value.startsWith(":")  => Keyword(Symbol(value.drop(1).trim))})
  }

  def string_atom: Parser[StringAtom] = {
    accept("string", { case sym @ StringToken(value) => StringAtom(value)})
  }

  def number_atom: Parser[NumberAtom] = {
    accept("number", { case sym @ NumberToken(value, radix) => NumberAtom(value, radix)})
  }

  def decimal_atom: Parser[DecimalAtom] = {
    accept("decimal", { case sym @ DecimalToken(value) => DecimalAtom(value)})
  }

  def nil_atom: Parser[NilAtom.type ] = {
    accept("nil", { case NilToken => NilAtom})
  }

  def atom: Parser[Atom] = symbol_atom | keyword_atom | string_atom | number_atom | decimal_atom | nil_atom | cons_cell | vector | syntactic_sugar
  def vector: Parser[VectorAtom] = LBracket ~> rep(atom) <~ RBracket ^^ { x => VectorAtom(x) }

  def syntactic_sugar: Parser[ConsCell] = quoted | backquoted | unquoted | exp_unquoted | list

  def cons_cell: Parser[ConsCell] = (LParen ~ atom ~ Dot ~ atom ~ RParen) ^^ { case _ ~ atom1 ~ _ ~ atom2 ~ _  => ConsCell(atom1, atom2) }

  def list: Parser[ConsCell] = (LParen ~ rep1( atom ) ~ RParen) ^^ { case _ ~ atoms ~ _ => ConsCell.mkCons(atoms).asInstanceOf[ConsCell] }

  def quoted = (Quote ~> atom) ^^ { case l => ConsCell(SymbolAtom('QUOTE), ConsCell(l, NilAtom)) }
  def backquoted = (Backquote ~> atom) ^^ { case l => ConsCell(SymbolAtom('QUASIQUOTE), ConsCell(l, NilAtom)) }
  def unquoted = (Unquote ~> atom) ^^ { case l => ConsCell(SymbolAtom('UNQUOTE), ConsCell(l, NilAtom)) }
  def exp_unquoted = (Unquote ~> At ~> atom) ^^ { case l => ConsCell(SymbolAtom(Symbol("UNQUOTE-EXPAND")), ConsCell(l, NilAtom)) }

  def program: Parser[List[Atom]] = phrase(rep(atom))

  def apply(tokens: Seq[Token]): Either[CactisLispParserError, List[Atom]] = {
    val reader = new CactisLispTokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(CactisLispParserError(msg))
      case Success(result, next) => Right(result)
    }
  }
}

object CactisLispCompiler {
  def apply(code: String): Either[CactisLispCompilationError, List[Atom]] = {
    for {
      tokens <- Lexer(code).right
      ast <- Parser(tokens.filter {
        case CommentToken(_) => false
        case _ => true
      }).right
    } yield ast
  }
}