package slick.ast

import scala.collection.mutable.ListBuffer
import scala.language.existentials
import scala.reflect.ClassTag
import slick.SlickException
import slick.util.{Dumpable, DumpInfo, ConstArray}

/** A node in the Slick AST */
trait Node extends Dumpable {
  def children: ConstArray[Node]
  def childNames: Iterable[String] = Stream.from(1).map(_.toString)

  def getDumpInfo = {
    val (objName, mainInfo) = this match {
      case p: Product =>
        val cln = DumpInfo.simpleNameFor(getClass)
        val n = if(cln.endsWith("$")) cln.substring(0, cln.length - 1) else cln.replaceFirst(".*\\$", "")
        val args = p.productIterator.filterNot(_.isInstanceOf[Node]).mkString(", ")
        (n, args)
      case _ => (super.toString, "")
    }
    DumpInfo(objName, mainInfo, "", childNames.zip(children.toSeq).toVector)
  }

  override final def toString = getDumpInfo.getNamePlusMainInfo
}

/** A Node which can be typed without access to its scope, and whose children can be typed
  * independently of each other. */
/** An expression that represents a conjunction of expressions. */
final case class ProductNode(children: ConstArray[Node]) extends Node {
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = "")
}

/** A literal value expression. */
case class LiteralNode(val value: Any) extends Node {
  def children = ConstArray.empty
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = s"$value")
}

/** A .filter call of type (CollectionType(c, t), Boolean) => CollectionType(c, t). */
final case class Filter(generator: Symbol, from: Node, where: Node) extends Node {
  lazy val children = ConstArray(from, where)
  override def childNames = Seq("from "+generator, "where")
}

final case class MapNode(generator: Symbol, from: Node, select: Node) extends Node {
  lazy val children = ConstArray(from, select)
  override def childNames = Seq("from "+generator, "select")
  def generators = ConstArray((generator, from))
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = "")
}

/** An expression that selects a field in another expression. */
final case class Select(in: Node, field: Symbol) extends Node {
  def sym = field
  lazy val children = ConstArray(in)
  override def childNames = Seq("in")
}

/** A function call expression. */
final case class Apply(sym: Symbol, children: ConstArray[Node])(val buildType: Type) extends Node {
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = sym.toString)
}

/** A reference to a Symbol */
final case class Ref(sym: Symbol) extends Node {
  def children = ConstArray.empty
}

/** A Node representing a database table. */
final case class TableNode(tableName: String)(val profileTable: Any) extends Node {
  def children = ConstArray.empty
  override def getDumpInfo = super.getDumpInfo.copy(name = "Table", mainInfo = tableName)
}

/** A client-side type mapping */
final case class TypeMapping(child: Node, mapper: MappedScalaType.Mapper, classTag: ClassTag[_]) extends Node {
  lazy val children = ConstArray(child)
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = "")
}
