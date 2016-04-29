package slick.lifted

import slick.ast._

/** Base class for all lifted table rows. */
abstract class Table[T](val tableTag: Tag, val tableName: String)
extends Rep[T] {
  def column[C : TypedType](n: String) = Rep[C](Select(toNode, Symbol(n)))

  def toNode = tableTag.toNode(TableNode(tableName))
  def encodeRef(path: Node) = tableTag.encodeRef(path).asInstanceOf[Table[T]]

  def * : ProvenShape[T]
}

/** A Tag marks a specific row represented by a Table instance. */
class Tag(cons: Tag => Table[_]) {
  def encodeRef(path: Node): Table[_] = cons(new Tag(cons) {
    override def toNode(n: Node): Node = path
  })
  def toNode(n: Node): Node = n
}
