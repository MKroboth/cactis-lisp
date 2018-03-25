package eu.cactis.cactis_lisp.ast

trait Atom {
  def repr: String

  override def equals(obj: scala.Any): Boolean = super.equals(obj)
  def <(obj: Atom): Boolean = false

  def +(obj: Atom): Atom = NilAtom
  def -(obj: Atom): Atom = NilAtom
  def *(obj: Atom): Atom = NilAtom
  def /(obj: Atom): Atom = NilAtom

}