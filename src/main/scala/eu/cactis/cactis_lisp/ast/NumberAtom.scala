package eu.cactis.cactis_lisp.ast

case class NumberAtom(value: BigInt, radix: Int = 10) extends Atom {
  override def repr: String = {
    val prefix = if (radix == 16) "0x" else ""

    val sign = if (value < 0) "-" else ""
    sign + prefix + value.toString(radix).dropWhile(_.toString == sign)
  }


  override def hashCode(): Int = value.hashCode()

  override def equals(obj: Any): Boolean = obj.isInstanceOf[NumberAtom] && obj.asInstanceOf[NumberAtom].value == value

  override def <(obj: Atom): Boolean = obj match {
    case x: NumberAtom => value < x.value
    case x: DecimalAtom => value < x.value.toBigInt()
  }

  override def +(obj: Atom): Atom = obj match {
    case NumberAtom(x, _) => NumberAtom(value + x)
    case DecimalAtom(x) => DecimalAtom(BigDecimal(value) + x)
    case _ => NilAtom
  }

  override def -(obj: Atom): Atom = obj match {
    case NumberAtom(x, _) => NumberAtom(value - x)
    case DecimalAtom(x) => DecimalAtom(BigDecimal(value) - x)
    case _ => NilAtom
  }

  override def *(obj: Atom): Atom = obj match {
    case NumberAtom(x, _) => NumberAtom(value * x)
    case DecimalAtom(x) => DecimalAtom(BigDecimal(value) * x)
    case _ => NilAtom
  }

  override def /(obj: Atom): Atom = obj match {
    case NumberAtom(x, _) => NumberAtom(value / x)
    case DecimalAtom(x) => DecimalAtom(BigDecimal(value) / x)
    case _ => NilAtom
  }


}