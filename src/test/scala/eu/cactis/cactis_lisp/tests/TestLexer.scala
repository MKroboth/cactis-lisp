package eu.cactis.cactis_lisp.tests
import org.scalatest._

import eu.cactis.cactis_lisp._

class TestLexer extends FlatSpec with Matchers {
  "The Lexer" should "tokenize everything correctly" in {
    Lexer("(test1 3 2 5 4.3 -0x32F)") should be(Right(List(LParen, IdentifierToken("test1"), NumberToken(3), NumberToken(2), NumberToken(5), DecimalToken(4.3), NumberToken(-815, 16), RParen)))
    CactisLispCompiler("(car '(cdr (1 2 3)))").right.map(_.head) should be(Right(LispUtils.fromNative(List('car, List('QUOTE, List('cdr, List(1, 2, 3)))))))
  }
}
