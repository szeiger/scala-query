package slick.lifted

import scala.language.existentials
import slick.ast._
import slick.SlickException

/** Common base trait for all lifted values, including columns. */
trait Rep[T] {
  /** Encode a reference into this Rep. */
  def encodeRef(path: Node): Rep[T]

  /** Get the Node for this Rep. */
  def toNode: Node
}

object Rep {
  def apply[T](n: Node): Rep[T] = new Rep[T] {
    def toNode = n
    def encodeRef(path: Node): Rep[T] = apply(path)
  }
}

/** A column with a constant value which is inserted into an SQL statement as a literal. */
final case class LiteralColumn[T](value: T) extends Rep[T] {
  val toNode = LiteralNode(value)
  def encodeRef(n: Node) = Rep(n)
}
