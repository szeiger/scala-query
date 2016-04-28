package slick.ast

import slick.util.ConstArray

/**
 * The standard library for query operators.
 */
object Library {
  // Boolean operators
  val And = new FunctionSymbol("and")
  val Or = new FunctionSymbol("or")
  val Not = new FunctionSymbol("not")

  // Numeric operators and functions
  val + = new FunctionSymbol("+")
  val - = new FunctionSymbol("-")

  // Comparison
  val < = new FunctionSymbol("<")
  val == = new FunctionSymbol("=")
}

/** A Symbol that represents a library function or operator */
class FunctionSymbol(val name: String) extends TermSymbol {

  /** Match an Apply of this Symbol */
  def unapplySeq(a: Apply) = if(a.sym eq this) Some(a.children.toSeq) else None

  /** Create a typed Apply of this Symbol */
  def typed(tpe: Type, ch: Node*): Apply = Apply(this, ConstArray.from(ch))(tpe)

  /** Create a typed Apply of this Symbol */
  def typed[T : ScalaBaseType](ch: Node*): Apply = Apply(this, ConstArray.from(ch))(implicitly[ScalaBaseType[T]])

  override def toString = "Function "+name
}
