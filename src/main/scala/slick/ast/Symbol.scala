package slick.ast

/** A symbol which can be used in the AST. */
case class Symbol(name: String) {
  override def toString = name
}
object Symbol {
  private var at = 0
  def fresh = { at += 1; Symbol(s"s$at") }
}
