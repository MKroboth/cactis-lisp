package eu.cactis.cactis_lisp

import eu.cactis.cactis_lisp.ast._

object LispUtils {
  def fromNative(obj: Any): Atom = obj match {
    case x: String => StringAtom(x)
    case x: Int => NumberAtom(x)
    case x: BigInt => NumberAtom(x)
    case x: BigDecimal => DecimalAtom(x)
    case x: Symbol => SymbolAtom(x)
    case x: List[Any] => ConsCell.mkCons(x.map(fromNative))
    case x: Map[Any, Any] => ConsCell.mkCons(x.toList.map(fromNative))
    case x: Atom => x
    case (x, y) => ConsCell(fromNative(x), fromNative(y))

    case _: Unit => NilAtom
    case _ => throw new RuntimeException("Unknown type")
  }

}
