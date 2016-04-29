package slick

import scala.language.implicitConversions
import slick.ast._

package object lifted {
  implicit def anyToShapedValue[T, U](value: T)(implicit shape: Shape[T, U, _]): ShapedValue[T, U] =
    new ShapedValue[T, U](value, shape)

  implicit class ColumnExtensionMethods[T : TypedType](private val n: Rep[T]) {
    def < (e: Rep[T]) = Rep[Boolean](Apply(Symbol("<"), Vector(n.toNode, e.toNode)))
    def === (e: Rep[T]) = Rep[Boolean](Apply(Symbol("=="), Vector(n.toNode, e.toNode)))
  }
}
