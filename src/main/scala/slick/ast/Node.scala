package slick.ast

/** A node in the Slick AST */
sealed trait Node

/** A tuple or other product */
final case class ProductNode(children: Vector[Node]) extends Node

/** A literal value */
final case class LiteralNode(value: Any) extends Node

/** A `map` call on a Query */
final case class MapNode(sym: Symbol, from: Node, select: Node) extends Node

/** A `filter` call on a Query */
final case class Filter(sym: Symbol, from: Node, where: Node) extends Node

/** A field selection */
final case class Select(in: Node, field: Symbol) extends Node

/** A function / operator call */
final case class Apply(f: Symbol, children: Vector[Node]) extends Node

/** A reference to a variable */
final case class Ref(sym: Symbol) extends Node

/** A reference to a database table */
final case class TableNode(name: String) extends Node

/** A client-side type mapping */
final case class TypeMapping(child: Node, toBase: Any => Any, toMapped: Any => Any) extends Node
