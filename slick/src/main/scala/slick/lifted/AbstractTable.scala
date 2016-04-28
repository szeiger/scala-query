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
trait BaseTag extends Tag

/** The profile-independent superclass of all table row objects.
  * @tparam T Row type for this table. Make sure it matches the type of your `*` projection. */
abstract class AbstractTable[T](val tableTag: Tag, val tableName: String) extends Rep[T] {
  /** The client-side type of the table as defined by its * projection */
  type TableElementType

  def tableIdentitySymbol: TableIdentitySymbol

  lazy val tableNode = TableNode(tableName, tableIdentitySymbol, tableIdentitySymbol)(this)

  def encodeRef(path: Node) = tableTag.taggedAs(path).asInstanceOf[AbstractTable[T]]

  /** The * projection of the table used as default for queries and inserts.
    * Should include all columns as a tuple, HList or custom shape and optionally
    * map them to a custom entity type using the <> operator.
    * The `ProvenShape` return type ensures that
    * there is a `Shape` available for translating between the `Column`-based
    * type in * and the client-side type without `Column` in the table's type
    * parameter. */
  def * : ProvenShape[T]

  override def toNode = tableTag match {
    case _: BaseTag =>
      val sym = new AnonSymbol
      TableExpansion(sym, tableNode, tableTag.taggedAs(Ref(sym)).*.toNode)
    case t: RefTag => t.path
  }
}

abstract class Table[T](_tableTag: Tag, _tableName: String) extends AbstractTable[T](_tableTag, _tableName) { table =>
  final type TableElementType = T

  def tableIdentitySymbol: TableIdentitySymbol = SimpleTableIdentitySymbol(tableName)

  def column[C](n: String)(implicit tt: TypedType[C]): Rep[C] = {
    new Rep.TypedRep[C] {
      override def toNode =
        Select((tableTag match {
          case r: RefTag => r.path
          case _ => tableNode
        }), FieldSymbol(n)(tt)) :@ tt
    }
  }
}
