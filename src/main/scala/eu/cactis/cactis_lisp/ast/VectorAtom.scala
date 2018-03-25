package eu.cactis.cactis_lisp.ast

case class VectorAtom(elemns: Seq[Atom]) extends Atom {
  override def repr: String = repr(x => x.repr)
  def repr(reprF: (Atom) => String): String = "[" + elemns.map(reprF).mkString(" ") + "]"

  override def equals(obj: Any): Boolean = obj.isInstanceOf[VectorAtom] && obj.asInstanceOf[VectorAtom].elemns.equals(elemns)

}
