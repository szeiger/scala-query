package slick.lifted

import slick.ast._

/** An instance of Query represents a query or view, i.e. a computation of a
  * collection type (Rep[Seq[T]]). It is parameterized with both, the mixed
  * type (the type of values you see e.g. when you call map()) and the unpacked
  * type (the type of values that you get back when you run the query). */
final class Query[+E, U](val toNode: Node, val shaped: ShapedValue[_ <: E, U]) extends Rep[Seq[U]] { self =>

  def filter(f: E => Rep[Boolean]): Query[E, U] = {
    val s = new AnonSymbol
    val fv = f(shaped.encodeRef(Ref(s)).value)
    new Query[E, U](Filter(s, toNode, fv.toNode), shaped)
  }

  def map[F, G, T](f: E => F)(implicit shape: Shape[F, T, G]): Query[G, T] = {
    val s = new AnonSymbol
    val fv = f(shaped.encodeRef(Ref(s)).value)
    val packed = ShapedValue(fv, shape).packedValue
    new Query[G, T](new MapNode(s, toNode, packed.toNode), packed)
  }

  def encodeRef(path: Node): Query[E, U] = new Query[E, U](path, self.shaped.encodeRef(path))
}

object TableQuery {
  /** Create a TableQuery for a table row class using an arbitrary constructor function. */
  def apply[E <: AbstractTable[_]](cons: Tag => E): Query[E, E#TableElementType] = {
    val shaped = ShapedValue(cons(new BaseTag(cons)), RepShape[E, E#TableElementType])
    new Query[E, E#TableElementType](shaped.toNode, shaped)
  }
}
