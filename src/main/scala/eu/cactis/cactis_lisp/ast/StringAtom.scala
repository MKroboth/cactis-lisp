package eu.cactis.cactis_lisp.ast

case class StringAtom(value: String) extends Atom {
  override def repr: String =  '"' + value + '"'
  override def equals(obj: Any): Boolean = obj.isInstanceOf[StringAtom] && obj.asInstanceOf[StringAtom].value == value
  override def <(obj: Atom): Boolean = obj.isInstanceOf[StringAtom] && (value.length < obj.asInstanceOf[StringAtom].value.length)

}