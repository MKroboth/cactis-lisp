package eu.cactis.cactis_lisp.ast

case object NilAtom extends Atom {
  override def repr: String = "()"

}
