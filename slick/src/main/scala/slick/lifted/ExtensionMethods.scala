package slick.lifted

import slick.util.ConstArray

import scala.language.{implicitConversions, higherKinds}
import slick.ast._
import FunctionSymbolExtensionMethods._
import ScalaBaseType._
import slick.SlickException

/** Extension methods for all columns */
final class ColumnExtensionMethods[B1](private val n: Rep[B1]) extends AnyVal {
  def === (e: Rep[B1]) = Rep.forNode[Boolean](Library.==.typed[Boolean](n.toNode, e.toNode))
  def < (e: Rep[B1]) = Rep.forNode[Boolean](Library.<.typed[Boolean](n.toNode, e.toNode))
}
