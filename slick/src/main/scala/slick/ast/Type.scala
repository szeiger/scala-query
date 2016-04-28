package slick.ast

import slick.util.ConstArray

trait Type {
  def children: ConstArray[Type]
}

final class MappedScalaType(val baseType: Type, val mapper: MappedScalaType.Mapper) extends Type {
  def children: ConstArray[Type] = ConstArray(baseType)
}

object MappedScalaType {
  case class Mapper(toBase: Any => Any, toMapped: Any => Any, fastPath: Option[Any => Any])
}

/** The standard type for freshly constructed nodes without an explicit type. */
case object UnassignedType extends Type {
  def children = ConstArray.empty
}

/** A Type that carries a primitive Scala type argument */
final class TypedType[T] extends Type {
  def children = ConstArray.empty
}

object TypedType {
  implicit val booleanType = new TypedType[Boolean]
  implicit val intType     = new TypedType[Int]
  implicit val stringType  = new TypedType[String]
}
