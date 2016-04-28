package slick.ast

import scala.collection.mutable.ListBuffer
import scala.language.existentials
import scala.reflect.ClassTag
import slick.SlickException
import slick.util.{Dumpable, DumpInfo, ConstArray}
import TypeUtil._

/** A node in the Slick AST.
  * Every Node has a number of child nodes and an optional type annotation. */
trait Node extends Dumpable {
  type Self >: this.type <: Node

  private[this] var seenType: Boolean = false
  private var _type: Type = UnassignedType

  /** All child nodes of this node. Must be implemented by subclasses. */
  def children: ConstArray[Node]

  /** Names for the child nodes to show in AST dumps. Defaults to a numbered sequence starting at 0
    * but can be overridden by subclasses to produce more suitable names. */
  def childNames: Iterable[String] = Stream.from(0).map(_.toString)

  /** Rebuild this node with a new list of children. Implementations of this method must not reuse
    * the current node. This method always returns a fresh copy. */
  protected[this] def rebuild(ch: ConstArray[Node]): Self

  /** Build a copy of this node with the current children. */
  protected[this] def buildCopy: Self = rebuild(children)

  /** Rebuild this node with new child nodes unless all children are identical to the current ones,
    * in which case this node is returned. */
  final def withChildren(ch2: ConstArray[Node]): Self = {
    val ch = children
    val len = ch.length
    var i = 0
    while(i < len) {
      if(ch(i) ne ch2(i)) return rebuild(ch2)
      i += 1
    }
    this
  }

  /** Apply a mapping function to all children of this node and recreate the node with the new
    * children. If all new children are identical to the old ones, this node is returned. If
    * ``keepType`` is true, the type of this node is kept even when the children have changed. */
  def mapChildren(f: Node => Node, keepType: Boolean = false): Self = {
    val ch = children
    val ch2 = ch.endoMap(f)
    val n: Self = if(ch2 eq ch) this else rebuild(ch2)
    if(!keepType || (_type eq UnassignedType)) n else (n :@ _type).asInstanceOf[Self]
  }

  /** Apply a side-effecting function to all direct children from left to right. Note that
    * {{{ n.childrenForeach(f) }}} is equivalent to {{{ n.children.foreach(f) }}} but can be
    * implemented more efficiently in `Node` subclasses. */
  def childrenForeach[R](f: Node => R): Unit =
    children.foreach(f)

  /** The current type of this node. */
  def nodeType: Type = {
    seenType = true
    _type
  }

  /** Get the current type of this node for debug output without marking it as seen. */
  protected[this] def peekType: Type = _type

  /** Check if this node has a type without marking the type as seen. */
  def hasType: Boolean = _type != UnassignedType

  /** Return this Node with no Type assigned (if it has not yet been observed) or an untyped copy. */
  final def untyped: Self =
    if(seenType || _type != UnassignedType) buildCopy else this

  /** Return this Node with a Type assigned (if no other type has been seen for it yet) or a typed copy. */
  final def :@ (newType: Type): Self = {
    val n: Self = if(seenType && newType != _type) buildCopy else this
    n._type = newType
    n
  }

  /** Rebuild this node and all children with their computed type. If this node already has a type,
    * the children are only type-checked again if ``typeChildren`` is true. if ``retype`` is also
    * true, the existing type of this node is replaced. If this node does not yet have a type, the
    * types of all children are computed first. */
  final def infer(scope: Type.Scope = Map.empty, typeChildren: Boolean = false): Self =
    if(hasType && !typeChildren) this else withInferredType(scope, typeChildren)

  protected[this] def withInferredType(scope: Type.Scope, typeChildren: Boolean): Self

  def getDumpInfo = {
    val (objName, mainInfo) = this match {
      case p: Product =>
        val cln = DumpInfo.simpleNameFor(getClass)
        val n = if(cln.endsWith("$")) cln.substring(0, cln.length - 1) else cln.replaceFirst(".*\\$", "")
        val args = p.productIterator.filterNot(_.isInstanceOf[Node]).mkString(", ")
        (n, args)
      case _ => (super.toString, "")
    }
    val t = peekType
    val ch = this match {
      // Omit path details unless dumpPaths is set
      case Path(l @ (_ :: _ :: _)) => Vector.empty
      case _ => childNames.zip(children.toSeq).toVector
    }
    DumpInfo(objName, mainInfo, if(t != UnassignedType) ": " + t.toString else "", ch)
  }

  override final def toString = getDumpInfo.getNamePlusMainInfo
}

/** A Node which can be typed without access to its scope, and whose children can be typed
  * independently of each other. */
trait SimplyTypedNode extends Node {
  type Self >: this.type <: SimplyTypedNode

  protected def buildType: Type

  final def withInferredType(scope: Type.Scope, typeChildren: Boolean): Self = {
    val this2: Self = mapChildren(_.infer(scope, typeChildren), keepType = true)
    if(!hasType) (this2 :@ this2.buildType).asInstanceOf[Self] else this2
  }
}

/** An expression that represents a conjunction of expressions. */
final case class ProductNode(children: ConstArray[Node]) extends SimplyTypedNode {
  type Self = ProductNode
  override def getDumpInfo = super.getDumpInfo.copy(name = "ProductNode", mainInfo = "")
  protected[this] def rebuild(ch: ConstArray[Node]): Self = copy(ch)
  override def childNames: Iterable[String] = Stream.from(1).map(_.toString)
  protected def buildType: Type = ProductType(children.map { ch =>
    val t = ch.nodeType
    if(t == UnassignedType) throw new SlickException(s"ProductNode child $ch has UnassignedType")
    t
  })
  def flatten: ProductNode = {
    def f(n: Node): ConstArray[Node] = n match {
      case ProductNode(ns) => ns.flatMap(f)
      case n => ConstArray(n)
    }
    ProductNode(f(this))
  }
}

/** A literal value expression.
  *
  * @param volatileHint Indicates whether this value should be considered volatile, i.e. it
  *                     contains user-generated data or may change in future executions of what
  *                     is otherwise the same query. A database back-end should usually turn
  *                     volatile constants into bind variables. */
class LiteralNode(val buildType: Type, val value: Any, val volatileHint: Boolean = false) extends NullaryNode with SimplyTypedNode {
  type Self = LiteralNode
  override def getDumpInfo = super.getDumpInfo.copy(name = "LiteralNode", mainInfo = s"$value (volatileHint=$volatileHint)")
  protected[this] def rebuild = new LiteralNode(buildType, value, volatileHint)

  override def hashCode = buildType.hashCode() + (if(value == null) 0 else value.asInstanceOf[AnyRef].hashCode)
  override def equals(o: Any) = o match {
    case l: LiteralNode => buildType == l.buildType && value == l.value
    case _ => false
  }
}

object LiteralNode {
  def apply(tp: Type, v: Any, vol: Boolean = false): LiteralNode = new LiteralNode(tp, v, vol)
  def apply[T](v: T)(implicit tp: ScalaBaseType[T]): LiteralNode = apply(tp, v)
  def unapply(n: LiteralNode): Option[Any] = Some(n.value)
}

trait BinaryNode extends Node {
  def left: Node
  def right: Node
  lazy val children = ConstArray(left, right)
  protected[this] final def rebuild(ch: ConstArray[Node]): Self = rebuild(ch(0), ch(1))
  protected[this] def rebuild(left: Node, right: Node): Self
  override final def mapChildren(f: Node => Node, keepType: Boolean = false): Self = {
    val l = left
    val r = right
    val l2 = f(l)
    val r2 = f(r)
    val n: Self = if((l eq l2) && (r eq r2)) this else rebuild(l2, r2)
    val _type = peekType
    if(!keepType || (_type eq UnassignedType)) n else (n :@ _type).asInstanceOf[Self]
  }
  override final protected[this] def buildCopy: Self = rebuild(left, right)
  override final def childrenForeach[R](f: Node => R): Unit = {
    f(left)
    f(right)
  }
}

trait UnaryNode extends Node {
  def child: Node
  lazy val children = ConstArray(child)
  protected[this] final def rebuild(ch: ConstArray[Node]): Self = rebuild(ch(0))
  protected[this] def rebuild(child: Node): Self
  override final def mapChildren(f: Node => Node, keepType: Boolean = false): Self = {
    val ch = child
    val ch2 = f(child)
    val n: Self = if(ch2 eq ch) this else rebuild(ch2)
    val _type = peekType
    if(!keepType || (_type eq UnassignedType)) n else (n :@ _type).asInstanceOf[Self]
  }
  override final protected[this] def buildCopy: Self = rebuild(child)
  override final def childrenForeach[R](f: Node => R): Unit = f(child)
}

trait NullaryNode extends Node {
  def children = ConstArray.empty
  protected[this] final def rebuild(ch: ConstArray[Node]): Self = rebuild
  protected[this] def rebuild: Self
  override final def mapChildren(f: Node => Node, keepType: Boolean = false): Self = this
  override final protected[this] def buildCopy: Self = rebuild
  override final def childrenForeach[R](f: Node => R): Unit = ()
}

/** An expression that represents a plain value lifted into a Query. */
final case class Pure(value: Node, identity: TypeSymbol = new AnonTypeSymbol) extends UnaryNode with SimplyTypedNode with TypeGenerator {
  type Self = Pure
  def child = value
  override def childNames = Seq("value")
  protected[this] def rebuild(child: Node) = copy(child)
  protected def buildType = CollectionType(TypedCollectionTypeConstructor.seq, NominalType(identity, value.nodeType))
}

final case class CollectionCast(child: Node, cons: CollectionTypeConstructor) extends UnaryNode with SimplyTypedNode {
  type Self = CollectionCast
  protected[this] def rebuild(child: Node) = copy(child = child)
  protected def buildType = CollectionType(cons, child.nodeType.asCollectionType.elementType)
  def nodeMapServerSide(keepType: Boolean, r: Node => Node) = mapChildren(r, keepType)
}

/** Common superclass for expressions of type (CollectionType(c, t), _) => CollectionType(c, t). */
abstract class FilteredQuery extends Node {
  type Self >: this.type <: FilteredQuery
  def from: Node
}

/** A FilteredQuery without a Symbol. */
abstract class SimpleFilteredQuery extends FilteredQuery with SimplyTypedNode {
  type Self >: this.type <: SimpleFilteredQuery
  def buildType = from.nodeType
}

/** A FilteredQuery with a Symbol. */
abstract class ComplexFilteredQuery extends FilteredQuery with DefNode {
  type Self >: this.type <: ComplexFilteredQuery
  protected[this] def generator: TermSymbol
  def generators = ConstArray((generator, from))
  def withInferredType(scope: Type.Scope, typeChildren: Boolean): Self = {
    val from2 = from.infer(scope, typeChildren)
    val genScope = scope + (generator -> from2.nodeType.asCollectionType.elementType)
    val this2 = mapChildren { ch =>
      if(ch eq from) from2 else ch.infer(genScope, typeChildren)
    }
    (this2 :@ (if(!hasType) this2.from.nodeType else nodeType)).asInstanceOf[Self]
  }
}

/** A .filter call of type (CollectionType(c, t), Boolean) => CollectionType(c, t). */
final case class Filter(generator: TermSymbol, from: Node, where: Node) extends ComplexFilteredQuery with BinaryNode {
  type Self = Filter
  def left = from
  def right = where
  override def childNames = Seq("from "+generator, "where")
  protected[this] def rebuild(left: Node, right: Node) = copy(from = left, where = right)
  protected[this] def rebuildWithSymbols(gen: ConstArray[TermSymbol]) = copy(generator = gen(0))
}

object Filter {
  def ifRefutable(generator: TermSymbol, from: Node, where: Node): Node = where match {
    case LiteralNode(true) => from
    case _ => Filter(generator, from, where)
  }
}

/** A .flatMap call of type
  * (CollectionType(c, _), CollectionType(_, u)) => CollectionType(c, u). */
final case class Bind(generator: TermSymbol, from: Node, select: Node) extends BinaryNode with DefNode {
  type Self = Bind
  def left = from
  def right = select
  override def childNames = Seq("from "+generator, "select")
  protected[this] def rebuild(left: Node, right: Node) = copy(from = left, select = right)
  def generators = ConstArray((generator, from))
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = "")
  protected[this] def rebuildWithSymbols(gen: ConstArray[TermSymbol]) = copy(generator = gen(0))
  def withInferredType(scope: Type.Scope, typeChildren: Boolean): Self = {
    val from2 = from.infer(scope, typeChildren)
    val from2Type = from2.nodeType.asCollectionType
    val select2 = select.infer(scope + (generator -> from2Type.elementType), typeChildren)
    val withCh = if((from2 eq from) && (select2 eq select)) this else rebuild(from2, select2)
    withCh :@ (
      if(!hasType) CollectionType(from2Type.cons, select2.nodeType.asCollectionType.elementType)
      else nodeType)
  }
}

trait PathElement extends Node {
  def sym: TermSymbol
  def pathString: String
  def untypedPath: PathElement
}

/** An expression that selects a field in another expression. */
final case class Select(in: Node, field: TermSymbol) extends PathElement with UnaryNode with SimplyTypedNode {
  def sym = field
  type Self = Select
  def child = in
  override def childNames = Seq("in")
  protected[this] def rebuild(child: Node) = copy(in = child)
  override def getDumpInfo = Path.unapply(this) match {
    case Some(l) => super.getDumpInfo.copy(name = "Path", mainInfo = l.reverseIterator.mkString("."))
    case None => super.getDumpInfo
  }
  protected def buildType = in.nodeType.select(field)
  def pathString = in.asInstanceOf[PathElement].pathString+"."+field
  def untypedPath = {
    val in2 = in.asInstanceOf[PathElement].untypedPath
    if(in2 eq in) untyped else Select(in2, field)
  }
}

/** A function call expression. */
final case class Apply(sym: TermSymbol, children: ConstArray[Node])(val buildType: Type) extends SimplyTypedNode {
  type Self = Apply
  protected[this] def rebuild(ch: ConstArray[slick.ast.Node]) = copy(children = ch)(buildType)
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = sym.toString)
}

/** A reference to a Symbol */
final case class Ref(sym: TermSymbol) extends PathElement with NullaryNode {
  type Self = Ref
  def withInferredType(scope: Type.Scope, typeChildren: Boolean): Self =
    if(hasType) this else {
      scope.get(sym) match {
        case Some(t) => this :@ t
        case _ => throw new SlickException("No type for symbol "+sym+" found for "+this)
      }
    }
  def rebuild = copy()
  def pathString = sym.toString
  def untypedPath = untyped
}

/** A constructor/extractor for nested Selects starting at a Ref so that, for example,
  * `c :: b :: a :: Nil` corresponds to path `a.b.c`. */
object Path {
  def apply(l: List[TermSymbol]): PathElement = l match {
    case s :: Nil => Ref(s)
    case s :: l => Select(apply(l), s)
    case _ => throw new SlickException("Empty Path")
  }
  def unapply(n: PathElement): Option[List[TermSymbol]] = {
    var l = new ListBuffer[TermSymbol]
    var el: Node = n
    while(el.isInstanceOf[Select]) {
      val sel = el.asInstanceOf[Select]
      l += sel.sym
      el = sel.child
    }
    el match {
      case Ref(sym) =>
        l += sym
        Some(l.toList)
      case _ => None
    }
  }
  def toString(path: Seq[TermSymbol]): String = path.reverseIterator.mkString("Path ", ".", "")
  def toString(s: Select): String = s match {
    case Path(syms) => toString(syms)
    case n => n.toString
  }
}

/** A constructor/extractor for nested Selects starting at a Ref so that, for example,
  * `a :: b :: c :: Nil` corresponds to path `a.b.c`. */
object FwdPath {
  def apply(ch: List[TermSymbol]): PathElement = {
    var p: PathElement = Ref(ch.head)
    ch.tail.foreach { sym => p = Select(p, sym) }
    p
  }
  def unapply(n: PathElement): Option[List[TermSymbol]] = {
    var l: List[TermSymbol] = Nil
    var el: Node = n
    while(el.isInstanceOf[Select]) {
      val sel = el.asInstanceOf[Select]
      l = sel.sym :: l
      el = sel.child
    }
    el match {
      case Ref(sym) => Some(sym :: l)
      case _ => None
    }
  }
  def toString(path: Seq[TermSymbol]): String = path.mkString("Path ", ".", "")
}

/** A Node representing a database table. */
final case class TableNode(tableName: String, identity: TableIdentitySymbol, baseIdentity: TableIdentitySymbol)(val profileTable: Any) extends NullaryNode with SimplyTypedNode with TypeGenerator {
  type Self = TableNode
  def buildType = CollectionType(TypedCollectionTypeConstructor.seq, NominalType(identity, UnassignedType))
  def rebuild = copy()(profileTable)
  override def getDumpInfo = super.getDumpInfo.copy(name = "Table", mainInfo = tableName)
}

/** A client-side type mapping */
final case class TypeMapping(child: Node, mapper: MappedScalaType.Mapper, classTag: ClassTag[_]) extends UnaryNode with SimplyTypedNode { self =>
  type Self = TypeMapping
  def rebuild(ch: Node) = copy(child = ch)
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = "")
  protected def buildType = new MappedScalaType(child.nodeType, mapper, classTag)
}
