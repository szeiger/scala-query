package slick.ast

import slick.ast.TypeUtil._
import slick.SlickException
import slick.util.ConstArray

/** Get the first element of a collection. For client-side operations only. */
final case class First(val child: Node) extends UnaryNode with SimplyTypedNode {
  type Self = First
  protected[this] def rebuild(ch: Node) = copy(child = ch)
  protected def buildType = child.nodeType.asCollectionType.elementType
}
