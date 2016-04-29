package slick.ast

/** A Type that carries a primitive Scala type argument */
final class TypedType[T]

object TypedType {
  implicit val booleanType = new TypedType[Boolean]
  implicit val intType     = new TypedType[Int]
  implicit val stringType  = new TypedType[String]
}
