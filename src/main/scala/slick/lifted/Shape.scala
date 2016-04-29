package slick.lifted

import scala.language.{existentials, implicitConversions, higherKinds}
import scala.annotation.implicitNotFound
import slick.util.{ProductWrapper, TupleSupport}
import slick.ast._

/** A type class that encodes the unpacking `Mixed => Unpacked` of a
 * `Query[Mixed]` to its result element type `Unpacked` and the packing to a
 * fully packed type `Packed`, i.e. a type where everything which is not a
 * transparent container is wrapped in a `Rep[_]`. */
@implicitNotFound(msg = "No matching Shape found.\nSlick does not know how to map the given types.\nPossible causes: T in Table[T] does not match your * projection. Or you use an unsupported type in a Query (e.g. scala List).\n     Source type: ${Mixed}\n   Unpacked type: ${Unpacked}\n     Packed type: ${Packed}\n")
trait Shape[-Mixed, Unpacked, Packed] {
  /** Encode a reference into a value of this Shape. */
  def encodeRef(value: Mixed, path: Node): Any

  /** Return an AST Node representing a mixed value. */
  def toNode(value: Mixed): Node

  /** Convert a value of this Shape's (mixed) type to the fully packed type */
  def pack(value: Mixed): Packed

  /** Return the fully packed Shape */
  def packedShape: Shape[Packed, Unpacked, Packed]
}

object Shape extends TupleShapeImplicits {
  implicit def primitiveShape[T : TypedType]: Shape[T, T, Rep[T]] = new Shape[T, T, Rep[T]] {
    def pack(value: T) = LiteralRep(value)
    def packedShape = repShape[Rep[T], T]
    def encodeRef(value: T, path: Node) =
      throw new RuntimeException("Shape does not have the same Mixed and Packed type")
    def toNode(value: T): Node = pack(value).toNode
  }

  implicit def columnShape[T : TypedType] = repShape[Rep[T], T]

  implicit def tableShape[T, C <: Table[_]](implicit ev: C <:< Table[T]) = repShape[C, T]

  implicit val unitShape: Shape[Unit, Unit, Unit] = new Shape[Unit, Unit, Unit] {
    def pack(value: Unit) = ()
    def packedShape: Shape[Unit, Unit, Unit] = this
    def encodeRef(value: Unit, path: Node) = ()
    def toNode(value: Unit) = ProductNode(Vector.empty)
  }

  /** Shape for Rep values (always fully packed) */
  def repShape[MP <: Rep[_], U]: Shape[MP, U, MP] = new Shape[MP, U, MP] {
    def pack(value: MP) = value
    def packedShape = this
    def encodeRef(value: MP, path: Node) = value.encodeRef(path)
    def toNode(value: MP) = value.toNode
  }
}

/** Base class for Shapes of record values which are represented by
  * ProductNodes in the AST.
  * @tparam C The supertype for the record values.
  */
abstract class ProductNodeShape[C, M <: C, U <: C, P <: C] extends Shape[M, U, P] {
  /** The Shapes for the product elements. */
  val shapes: Seq[Shape[_, _, _]]

  /** Build a record value represented by this Shape from its element values. */
  def buildValue(elems: IndexedSeq[Any]): Any

  /** Create a copy of this Shape with new element Shapes. This is used for
    * packing Shapes recursively. */
  def copy(shapes: Seq[Shape[_, _, _]]): Shape[_, _, _]

  /** Get the element value from a record value at the specified index. */
  def getElement(value: C, idx: Int): Any

  /** Get an Iterator of a record value's element values. The default
    * implementation repeatedly calls `getElement`. */
  def getIterator(value: C): Iterator[Any] =
    shapes.iterator.zipWithIndex.map(t => getElement(value, t._2))

  def pack(value: M) = {
    val elems = shapes.iterator.zip(getIterator(value)).map{ case (p, f) => p.asInstanceOf[Shape[Any, Any, Any]].pack(f) }
    buildValue(elems.toIndexedSeq).asInstanceOf[P]
  }
  def packedShape: Shape[P, U, P] =
    copy(shapes.map(_.packedShape)).asInstanceOf[Shape[P, U, P]]
  def toNode(value: M): Node =
    ProductNode(shapes.iterator.zip(getIterator(value)).map {
      case (p, f) => p.asInstanceOf[Shape[Any, Any, Any]].toNode(f)
    }.toVector)
  def encodeRef(value: M, path: Node) =
    buildValue(shapes.iterator.zip(getIterator(value)).zipWithIndex.map {
      case ((p, x), pos) => p.asInstanceOf[Shape[Any, Any, Any]].encodeRef(x, Select(path, Symbol("_" + (pos+1))))
    }.toIndexedSeq)
}

/** Base class for ProductNodeShapes with a type mapping to a type that extends scala.Product */
abstract class MappedScalaProductShape[C <: Product, M <: C, U <: C, P <: C] extends ProductNodeShape[C, M, U, P] {
  override def toNode(value: M) = TypeMapping(super.toNode(value), toBase, toMapped)
  def toBase(v: Any) = new ProductWrapper(getIterator(v.asInstanceOf[C]).toIndexedSeq)
  def toMapped(v: Any) = buildValue(TupleSupport.buildIndexedSeq(v.asInstanceOf[Product]))
  def getElement(value: C, idx: Int) = value.productElement(idx)
}

/** Shape for Scala tuples of all arities */
final class TupleShape[M <: Product, U <: Product, P <: Product](val shapes: Shape[_, _, _]*) extends ProductNodeShape[Product, M, U, P] {
  def getElement(value: Product, idx: Int) = value.productElement(idx)
  def buildValue(elems: IndexedSeq[Any]) = TupleSupport.buildTuple(elems)
  def copy(shapes: Seq[Shape[_, _, _]])  = new TupleShape(shapes: _*)
}

/** A generic case class shape that can be used to lift a case class of
  * plain Scala types to a case class of lifted types. This allows the type
  * to be used as a record type (like tuples and HLists) in the Lifted
  * Embedding. */
class CaseClassShape[P <: Product, LiftedTuple, LiftedCaseClass <: P, PlainTuple, PlainCaseClass <: P](
   mapLifted: LiftedTuple => LiftedCaseClass, mapPlain: PlainTuple => PlainCaseClass)(
   implicit columnShapes: Shape[LiftedTuple, PlainTuple, LiftedTuple])
extends MappedScalaProductShape[P, LiftedCaseClass, PlainCaseClass, LiftedCaseClass] {
  val shapes = columnShapes.asInstanceOf[TupleShape[_,_,_]].shapes
  override def toMapped(v: Any) = mapPlain(v.asInstanceOf[PlainTuple])
  def buildValue(elems: IndexedSeq[Any]) = mapLifted(TupleSupport.buildTuple(elems).asInstanceOf[LiftedTuple])
  def copy(s: Seq[Shape[_, _, _]]) = new CaseClassShape(mapLifted, mapPlain) { override val shapes = s }
}

/** A value together with its Shape */
case class ShapedValue[T, U](value: T, shape: Shape[T, U, _]) extends Rep[U] {
  def encodeRef(path: Node): ShapedValue[T, U] = new ShapedValue(shape.encodeRef(value, path).asInstanceOf[T], shape)
  def toNode = shape.toNode(value)
  def packedValue[R](implicit ev: Shape[T, _, R]): ShapedValue[R, U] = ShapedValue(shape.pack(value).asInstanceOf[R], shape.packedShape.asInstanceOf[Shape[R, U, _]])
  def <>[R](f: (U => R), g: (R => Option[U])) = new MappedProjection[R, U](shape.toNode(value), g.andThen(_.get).asInstanceOf[Any => Any], f.asInstanceOf[Any => Any])
}

object ShapedValue {
  implicit def shapedValueShape[T, U] = Shape.repShape[ShapedValue[T, U], U]
}

/** A limited version of ShapedValue which can be constructed for every type
  * that has a valid shape. We use it to enforce that a table's * projection
  * has a valid shape. A ProvenShape has itself a Shape so it can be used in
  * place of the value that it wraps for purposes of packing and unpacking. */
trait ProvenShape[U] {
  def value: Any
  val shape: Shape[_, U, _]
  def packedValue[R](implicit ev: Shape[_, U, R]): ShapedValue[R, U]
  def toNode = packedValue(shape).toNode
}

object ProvenShape {
  /** Convert an appropriately shaped value to a ProvenShape */
  implicit def proveShapeOf[T, U](v: T)(implicit sh: Shape[T, U, _]): ProvenShape[U] =
    new ProvenShape[U] {
      def value = v
      val shape: Shape[_, U, _] = sh.asInstanceOf[Shape[_, U, _]]
      def packedValue[R](implicit ev: Shape[_, U, R]): ShapedValue[R, U] = ShapedValue(sh.pack(value).asInstanceOf[R], sh.packedShape.asInstanceOf[Shape[R, U, _]])
    }
}

class MappedProjection[T, P](child: Node, toBase: Any => Any, toMapped: Any => Any) extends Rep[T] {
  type Self = MappedProjection[_, _]
  override def toString = "MappedProjection"
  override def toNode: Node = TypeMapping(child, toBase, toMapped)
  def encodeRef(path: Node): MappedProjection[T, P] = new MappedProjection[T, P](child, toBase, toMapped) {
    override def toNode = path
  }
}

object MappedProjection {
  /** The Shape for a MappedProjection */
  implicit final def mappedProjectionShape[T, P] = Shape.repShape[MappedProjection[T, P], T]
}
