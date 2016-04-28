package slick.lifted

import slick.util.ConstArray

import scala.language.higherKinds
import scala.language.experimental.macros
import scala.annotation.implicitNotFound
import slick.ast._
import FunctionSymbolExtensionMethods._
import ScalaBaseType._

sealed trait QueryBase[T] extends Rep[T]

/** An instance of Query represents a query or view, i.e. a computation of a
  * collection type (Rep[Seq[T]]). It is parameterized with both, the mixed
  * type (the type of values you see e.g. when you call map()) and the unpacked
  * type (the type of values that you get back when you run the query).
  *
  * Additional extension methods for queries containing a single column are
  * defined in [[slick.lifted.SingleColumnQueryExtensionMethods]].
  */
sealed abstract class Query[+E, U, C[_]] extends QueryBase[C[U]] { self =>
  def shaped: ShapedValue[_ <: E, U]
  final lazy val packed = shaped.toNode

  /** Build a new query by applying a function to all elements of this query
    * and using the elements of the resulting queries. This corresponds to an
    * implicit inner join in SQL. */
  def flatMap[F, T, D[_]](f: E => Query[F, T, D]): Query[F, T, C] = {
    val generator = new AnonSymbol
    val aliased = shaped.encodeRef(Ref(generator)).value
    val fv = f(aliased)
    new WrappingQuery[F, T, C](new Bind(generator, toNode, fv.toNode), fv.shaped)
  }

  /** Build a new query by applying a function to all elements of this query. */
  def map[F, G, T](f: E => F)(implicit shape: Shape[_ <: FlatShapeLevel, F, T, G]): Query[G, T, C] =
    flatMap(v => Query[F, T, G](f(v)))

  /** Select all elements of this query which satisfy a predicate. */
  private def filterHelper[T](f: E => T, wrapExpr: Node => Node)
                             (implicit wt: CanBeQueryCondition[T]): Query[E, U, C] = {
    val generator = new AnonSymbol
    val aliased = shaped.encodeRef(Ref(generator))
    val fv = f(aliased.value)
    new WrappingQuery[E, U, C](Filter.ifRefutable(generator, toNode, wrapExpr(wt(fv).toNode)), shaped)
  }
  /** Select all elements of this query which satisfy a predicate. Unlike
    * `withFilter, this method only allows `Rep`-valued predicates, so it
    * guards against the accidental use use plain Booleans. */
  def filter[T <: Rep[_]](f: E => T)(implicit wt: CanBeQueryCondition[T]): Query[E, U, C] =
    withFilter(f)
  def filterNot[T <: Rep[_]](f: E => T)(implicit wt: CanBeQueryCondition[T]): Query[E, U, C] =
    filterHelper(f, node => Library.Not.typed(node.nodeType, node) )

  /** Select all elements of this query which satisfy a predicate. This method
    * is used when desugaring for-comprehensions over queries. There is no
    * reason to call it directly because it is the same as `filter`. */
  def withFilter[T : CanBeQueryCondition](f: E => T) = filterHelper(f, identity)

  def encodeRef(path: Node): Query[E, U, C] = new Query[E, U, C] {
    val shaped = self.shaped.encodeRef(path)
    def toNode = path
  }

  def pack[R](implicit packing: Shape[_ <: FlatShapeLevel, E, _, R]): Query[R, U, C] =
    new Query[R, U, C] {
      val shaped: ShapedValue[_ <: R, U] = self.shaped.packedValue(packing)
      def toNode = self.toNode
    }
}

/** The companion object for Query contains factory methods for creating queries. */
object Query {
  /** Lift a scalar value to a Query. */
  def apply[E, U, R](value: E)(implicit unpack: Shape[_ <: FlatShapeLevel, E, U, R]): Query[R, U, Seq] = {
    val shaped = ShapedValue(value, unpack).packedValue
    new WrappingQuery[R, U, Seq](Pure(shaped.toNode), shaped)
  }

  /** The empty Query. */
  def empty: Query[Unit, Unit, Seq] = new Query[Unit, Unit, Seq] {
    val toNode = shaped.toNode
    def shaped = ShapedValue((), Shape.unitShape[FlatShapeLevel])
  }

  @inline implicit def queryShape[Level >: NestedShapeLevel <: ShapeLevel, T, Q <: QueryBase[_]](implicit ev: Q <:< Rep[T]) = RepShape[Level, Q, T]
}

/** A typeclass for types that can be used as predicates in `filter` calls. */
trait CanBeQueryCondition[-T] extends (T => Rep[_])

object CanBeQueryCondition {
  implicit val BooleanColumnCanBeQueryCondition : CanBeQueryCondition[Rep[Boolean]] =
    new CanBeQueryCondition[Rep[Boolean]] {
      def apply(value: Rep[Boolean]) = value
    }
  implicit val BooleanCanBeQueryCondition : CanBeQueryCondition[Boolean] =
    new CanBeQueryCondition[Boolean] {
      def apply(value: Boolean) = new LiteralColumn(value)
    }
}

class WrappingQuery[+E, U, C[_]](val toNode: Node, val shaped: ShapedValue[_ <: E, U]) extends Query[E, U, C]

/** Represents a database table. Profiles add extension methods to TableQuery
  * for operations that can be performed on tables but not on arbitrary
  * queries, e.g. getting the table DDL. */
class TableQuery[E <: AbstractTable[_]](cons: Tag => E) extends Query[E, E#TableElementType, Seq] {
  lazy val shaped = {
    val baseTable = cons(new BaseTag { base =>
      def taggedAs(path: Node): AbstractTable[_] = cons(new RefTag(path) {
        def taggedAs(path: Node) = base.taggedAs(path)
      })
    })
    ShapedValue(baseTable, RepShape[FlatShapeLevel, E, E#TableElementType])
  }

  lazy val toNode = shaped.toNode

  /** Get the "raw" table row that represents the table itself, as opposed to
    * a Path for a variable of the table's type. This method should generally
    * not be called from user code. */
  def baseTableRow: E = shaped.value
}

object TableQuery {
  /** Create a TableQuery for a table row class using an arbitrary constructor function. */
  def apply[E <: AbstractTable[_]](cons: Tag => E): TableQuery[E] =
    new TableQuery[E](cons)
}
