package slick.lifted

import slick.ast._

/** A Tag marks a specific row represented by an AbstractTable instance. */
sealed trait Tag {
  /** Return a new instance of the AbstractTable carrying this Tag, with a new path */
  def taggedAs(path: Node): AbstractTable[_]
}

/** A Tag for table instances that represent a Node */
abstract class RefTag(val path: Node) extends Tag

/** A Tag marking the base table instance itself */
class BaseTag(cons: Tag => AbstractTable[_]) extends Tag { base =>
  def taggedAs(path: Node): AbstractTable[_] = cons(new RefTag(path) {
    def taggedAs(path: Node) = base.taggedAs(path)
  })
}

/** The profile-independent superclass of all table row objects.
  * @tparam T Row type for this table. Make sure it matches the type of your `*` projection. */
abstract class AbstractTable[T](val tableTag: Tag, val tableName: String) extends Rep[T] {
  /** The client-side type of the table as defined by its * projection */
  type TableElementType

  lazy val tableNode = TableNode(tableName)(this)

  def encodeRef(path: Node) = tableTag.taggedAs(path).asInstanceOf[AbstractTable[T]]

  def * : ProvenShape[T]

  override def toNode = tableTag match {
    case _: BaseTag => tableNode
    case t: RefTag => t.path
  }
}

abstract class Table[T](_tableTag: Tag, _tableName: String) extends AbstractTable[T](_tableTag, _tableName) { table =>
  final type TableElementType = T

  def column[C : TypedType](n: String) = Rep[C](Select(table.toNode, FieldSymbol(n)))
}
