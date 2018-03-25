package eu.cactis.cactis_lisp.ast

case class ConsCell(ar: Atom, dr: Atom) extends Atom {
  override def repr: String = repr(x => x.repr)

  def repr(reprF: (Atom) => String): String = dr match { case _: ConsCell =>

    def consCellRepr(cs: ConsCell): List[String] = (cs.ar match {
      case cell: ConsCell if (cell.ar.isInstanceOf[SymbolAtom] || cell.ar.isInstanceOf[StringAtom]) && cell.dr == NilAtom => s"(${reprF(cell.ar)} . ())"
      case _ => reprF(cs.ar)
    }) :: (cs.dr match {
      case x: ConsCell => consCellRepr(x)
      case NilAtom => Nil
      case x: Atom => reprF(x) :: Nil
    })

    "("+consCellRepr(this).mkString(" ")+")"

  case NilAtom => s"(${reprF(ar)})"
  case _ => s"(${reprF(ar)} . ${reprF(dr)})"
  }

  def toList: List[Atom] = {
    ar :: (dr match {
      case NilAtom => Nil
      case cell: ConsCell => cell.toList
      case _ => dr :: Nil
    })
  }

  override def equals(obj: Any): Boolean = obj.isInstanceOf[ConsCell] && obj.asInstanceOf[ConsCell].toList.equals(toList)

}

object ConsCell {
  def mkCons(xs: List[Atom]): Atom = xs match {
    case Nil => NilAtom
    case x :: rest => ConsCell(x, mkCons(rest))
  }
}