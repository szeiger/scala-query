package slick
package lifted

import slick.ast.{Library, TypedType}

import scala.language.higherKinds
import scala.language.implicitConversions

object API {
  implicit final def anyToShapedValue[T, U](value: T)(implicit shape: Shape[T, U, _]): ShapedValue[T, U] =
    new ShapedValue[T, U](value, shape)

  implicit class ColumnExtensionMethods[B1](private val n: Rep[B1]) extends AnyVal {
    def === (e: Rep[B1]) = Rep[Boolean](Library.==.typed[Boolean](n.toNode, e.toNode))
    def < (e: Rep[B1]) = Rep[Boolean](Library.<.typed[Boolean](n.toNode, e.toNode))
  }
}
