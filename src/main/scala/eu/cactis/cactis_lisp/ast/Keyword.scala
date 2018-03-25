package eu.cactis.cactis_lisp.ast

case class Keyword(value: scala.Symbol) extends Atom {
  override def repr: String = s":${value.name}"

  override def equals(obj: Any): Boolean = obj.isInstanceOf[Keyword] && obj.asInstanceOf[Keyword].value == value
}
