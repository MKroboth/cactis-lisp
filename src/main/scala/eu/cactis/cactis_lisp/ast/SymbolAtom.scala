package eu.cactis.cactis_lisp.ast

case class SymbolAtom(value: scala.Symbol) extends Atom {
  require(value.name.trim == value.name)
  require(!value.name.contains(" "))

  override def repr: String = value.name

  override def equals(obj: Any): Boolean = obj.isInstanceOf[SymbolAtom] && obj.asInstanceOf[SymbolAtom].value == value
}
