package slick.ast

import slick.util.ConstArray
import scala.collection.mutable.HashMap
import scala.reflect.ClassTag
import scala.util.DynamicVariable

/** A symbol which can be used in the AST. */
trait Symbol {
  def name: String
  override def toString = SymbolNamer(this)
}

/** A named symbol which refers to an (aliased or unaliased) field. */
case class FieldSymbol(name: String) extends Symbol

/** An element of a ProductNode (using a 1-based index) */
case class ElementSymbol(idx: Int) extends Symbol {
  def name = "_" + idx
}

/** An anonymous symbol defined in the AST. */
class AnonSymbol extends Symbol {
  def name = "@"+System.identityHashCode(this)
}

/** Provides names for symbols */
class SymbolNamer(treeSymbolPrefix: String, parent: Option[SymbolNamer] = None) {
  private var curSymbolId = 1
  private val map = new HashMap[Symbol, String]

  def create(prefix: String) = {
    curSymbolId += 1
    prefix + curSymbolId
  }

  def get(s: Symbol): Option[String] =
    map.get(s) orElse parent.flatMap(_.get(s))

  def apply(s: Symbol): String = get(s).getOrElse(s match {
    case a: AnonSymbol =>
      val n = create(treeSymbolPrefix)
      update(a, n)
      n
    case s => namedSymbolName(s)
  })

  def namedSymbolName(s: Symbol) = s.name

  def update(s: Symbol, n: String): Unit = map += s -> n

  def use[T](f: => T): T = SymbolNamer.dyn.withValue(this)(f)
}

object SymbolNamer {
  private val dyn = new DynamicVariable[SymbolNamer](null)
  def apply(s: Symbol): String = {
    val n = dyn.value
    if(n eq null) s.name else n(s)
  }
}
