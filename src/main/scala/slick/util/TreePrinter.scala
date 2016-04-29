package slick.util

import slick.ast._

/** Create a readable printout of an AST. */
object TreePrinter {
  def apply(n: Node, prefix1: String = "", prefix2: String = "", name: String = "") {
    val (ns: String, children: Vector[(String, Node)]) = n match {
      case ProductNode(ch) => ("Product", ch.zipWithIndex.map { case (n, i) => ((i+1).toString, n) }.toVector)
      case LiteralNode(v) => (s"Literal $v", Vector.empty)
      case MapNode(sym, from, s) => ("Map", Vector((s"from $sym", from), ("select", s)))
      case Filter(sym, from, p) => ("Filter", Vector((s"from $sym", from), ("where", p)))
      case Select(in, f) => (s"Select $f", Vector(("in", in)))
      case Apply(sym, ch) => (s"Apply $sym", ch.zipWithIndex.map { case (n, i) => (i.toString, n) }.toVector)
      case Ref(sym) => (s"Ref $sym", Vector.empty)
      case TableNode(name) => (s"Table $name", Vector.empty)
      case TypeMapping(ch, _, _) => ("TypeMapping", Vector(("0", ch)))
      case n => (s"??? $n", Nil)
    }
    println(prefix1 + "\u001B[36m" + name + (if(name.isEmpty) "" else ": ") + "\u001B[0m" + ns)
    children.zipWithIndex.foreach { case ((name, n), idx) =>
      val (p1, p2) = if(idx == children.size-1) ("\u2517 ", "  ") else ("\u2523 ", "\u2503 ")
      apply(n, prefix2 + "\u001B[34m" + p1, prefix2 + "\u001B[34m" + p2, name)
    }
  }
}
