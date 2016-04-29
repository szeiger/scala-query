package slick.lifted

import slick.ast._

/** Common base trait for all lifted values. */
trait Rep[T] {
  /** Get the Node for this Rep. */
  def toNode: Node

  /** Encode a reference into this Rep. */
  def encodeRef(path: Node): Rep[T]
}

object Rep {
  def apply[T](n: Node): Rep[T] = new Rep[T] {
    def toNode = n
    def encodeRef(path: Node): Rep[T] = apply(path)
  }
}

/** A lifted literal value. */
final case class LiteralRep[T : TypedType](value: T) extends Rep[T] {
  val toNode = LiteralNode(value)
  def encodeRef(n: Node) = Rep(n)
}

/*
implicit class AnyRepExtensionMethods[T1](private val v1: Rep[T1]) {
  def ~ [T2](v2: Rep[T2]) = RepTuple2[T1, T2](v1, v2)
}

case class RepTuple2[T1, T2](v1: Rep[T1], v2: Rep[T2]) extends Rep[(T1, T2)] {
  def toNode: Node = ProductNode(Vector(v1.toNode, v2.toNode))
  def encodeRef(path: Node) = new RepTuple2(v1, v2) {
    override def toNode = path
  }
  def ~ [T3](v3: Rep[T3]) = RepTuple3[T1, T2, T3](v1, v2, v3)
}
*/
