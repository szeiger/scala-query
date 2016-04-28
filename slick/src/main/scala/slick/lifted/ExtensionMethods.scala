package slick.lifted

import slick.util.ConstArray

import scala.language.{implicitConversions, higherKinds}
import slick.ast._
import FunctionSymbolExtensionMethods._
import ScalaBaseType._
import slick.SlickException

trait ExtensionMethods[B1, P1] extends Any {
  protected[this] def c: Rep[P1]
  @inline protected[this] def n = c.toNode
  @inline protected[this] def tpe[T](r: Rep[T]): TypedType[T] = r.asInstanceOf[Rep.TypedRep[_]].tpe.asInstanceOf[TypedType[T]]
  @inline protected[this] implicit def p1Type = tpe(c)
  protected[this] implicit def b1Type: TypedType[B1]
  protected[this] type o = OptionMapperDSL.arg[B1, P1]
}

trait BaseExtensionMethods[B1] extends Any with ExtensionMethods[B1, B1] {
  protected[this] implicit def b1Type = p1Type
}

trait OptionExtensionMethods[B1] extends Any with ExtensionMethods[B1, Option[B1]] {
  protected[this] implicit def b1Type = p1Type.asInstanceOf[OptionType].elementType.asInstanceOf[TypedType[B1]]
}

/** Extension methods for all columns */
trait ColumnExtensionMethods[B1, P1] extends Any with ExtensionMethods[B1, P1] {
  def === [P2, R](e: Rep[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om.column(Library.==, n, e.toNode)
  def =!= [P2, R](e: Rep[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om.column(Library.Not, Library.==.typed(om.liftedType, n, e.toNode))

  def < [P2, R](e: Rep[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om.column(Library.<, n, e.toNode)
}

final class BaseColumnExtensionMethods[P1](val c: Rep[P1]) extends AnyVal with ColumnExtensionMethods[P1, P1] with BaseExtensionMethods[P1] {
  /** Lift a column to an Option column. This is the same as calling [[slick.lifted.Rep.Some]]. */
  def ? : Rep[Option[P1]] = Rep.forNode(OptionApply(c.toNode))(p1Type.optionType)
}

final class OptionColumnExtensionMethods[B1](val c: Rep[Option[B1]]) extends AnyVal with ColumnExtensionMethods[B1, Option[B1]] with OptionExtensionMethods[B1] {
  /** Get the value inside this Option, if it is non-empty, otherwise throw a SlickException. This
    * operation is only allowed in places where it can be performed at the client side (e.g. not
    * inside a subquery that cannot be fused), otherwise the exception is thrown during query
    * compilation. */
  def get: Rep[B1] =
    Rep.forNode[B1](GetOrElse(c.toNode, () => throw new SlickException("Read NULL value for column " + this)))(c.asInstanceOf[Rep.TypedRep[_]].tpe.asInstanceOf[OptionType].elementType.asInstanceOf[TypedType[B1]])
}

/** Extension methods for Rep[Boolean] and Rep[Option[Boolean]] */
final class BooleanColumnExtensionMethods[P1](val c: Rep[P1]) extends AnyVal with ExtensionMethods[Boolean, P1] {
  protected[this] implicit def b1Type = implicitly[TypedType[Boolean]]

  def &&[P2, R](b: Rep[P2])(implicit om: o#arg[Boolean, P2]#to[Boolean, R]) =
    om.column(Library.And, n, b.toNode)
  def ||[P2, R](b: Rep[P2])(implicit om: o#arg[Boolean, P2]#to[Boolean, R]) =
    om.column(Library.Or, n, b.toNode)
  def unary_! = Library.Not.column[P1](n)
}

trait ExtensionMethodConversions {
  implicit def columnExtensionMethods[B1 : BaseTypedType](c: Rep[B1]): BaseColumnExtensionMethods[B1] = new BaseColumnExtensionMethods[B1](c)
  implicit def optionColumnExtensionMethods[B1 : BaseTypedType](c: Rep[Option[B1]]): OptionColumnExtensionMethods[B1] = new OptionColumnExtensionMethods[B1](c)
  implicit def booleanColumnExtensionMethods(c: Rep[Boolean]): BooleanColumnExtensionMethods[Boolean] = new BooleanColumnExtensionMethods[Boolean](c)
  implicit def booleanOptionColumnExtensionMethods(c: Rep[Option[Boolean]]): BooleanColumnExtensionMethods[Option[Boolean]] = new BooleanColumnExtensionMethods[Option[Boolean]](c)
}
