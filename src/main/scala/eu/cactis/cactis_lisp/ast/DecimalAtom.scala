package eu.cactis.cactis_lisp.ast

case class DecimalAtom(value: BigDecimal) extends Atom {
  override def repr: String = value.toString()

  override def equals(obj: Any): Boolean = obj.isInstanceOf[DecimalAtom] && obj.asInstanceOf[DecimalAtom].value == value

  override def <(obj: Atom): Boolean = obj match {
    case x: NumberAtom => value.toBigInt()  < x.value
    case x: DecimalAtom => value < x.value
  }

  override def +(obj: Atom): Atom = obj match {
    case NumberAtom(x, _) => DecimalAtom(value + BigDecimal(x))
    case DecimalAtom(x) => DecimalAtom(value + x)
    case _ => NilAtom
  }

  override def -(obj: Atom): Atom = obj match {
    case NumberAtom(x, _) => DecimalAtom(value - BigDecimal(x))
    case DecimalAtom(x) => DecimalAtom(value - x)
    case _ => NilAtom
  }

  override def *(obj: Atom): Atom = obj match {
    case NumberAtom(x, _) => DecimalAtom(value * BigDecimal(x))
    case DecimalAtom(x) => DecimalAtom(value * x)
    case _ => NilAtom
  }

  override def /(obj: Atom): Atom = obj match {
    case NumberAtom(x, _) => DecimalAtom(value / BigDecimal(x))
    case DecimalAtom(x) => DecimalAtom(value / x)
    case _ => NilAtom
  }
}