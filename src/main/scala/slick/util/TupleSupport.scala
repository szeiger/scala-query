// This file is usually generated with fmpp

package slick.util

import scala.language.implicitConversions

/** Utility functions for working with tuples of different arities */
object TupleSupport {
  /** Build a Tuple for the supported arities, otherwise a ProductWrapper. */
  def buildTuple(s: IndexedSeq[Any]): Product = s.length match {
    case 1 => new Tuple1(s(0))
    case 2 => new Tuple2(s(0), s(1))
    case 3 => new Tuple3(s(0), s(1), s(2))
    case 4 => new Tuple4(s(0), s(1), s(2), s(3))
    case 5 => new Tuple5(s(0), s(1), s(2), s(3), s(4))
    case 6 => new Tuple6(s(0), s(1), s(2), s(3), s(4), s(5))
    case 7 => new Tuple7(s(0), s(1), s(2), s(3), s(4), s(5), s(6))
    case 8 => new Tuple8(s(0), s(1), s(2), s(3), s(4), s(5), s(6), s(7))
    case 9 => new Tuple9(s(0), s(1), s(2), s(3), s(4), s(5), s(6), s(7), s(8))
    case 10 => new Tuple10(s(0), s(1), s(2), s(3), s(4), s(5), s(6), s(7), s(8), s(9))
    case 11 => new Tuple11(s(0), s(1), s(2), s(3), s(4), s(5), s(6), s(7), s(8), s(9), s(10))
    case 12 => new Tuple12(s(0), s(1), s(2), s(3), s(4), s(5), s(6), s(7), s(8), s(9), s(10), s(11))
    case 13 => new Tuple13(s(0), s(1), s(2), s(3), s(4), s(5), s(6), s(7), s(8), s(9), s(10), s(11), s(12))
    case 14 => new Tuple14(s(0), s(1), s(2), s(3), s(4), s(5), s(6), s(7), s(8), s(9), s(10), s(11), s(12), s(13))
    case 15 => new Tuple15(s(0), s(1), s(2), s(3), s(4), s(5), s(6), s(7), s(8), s(9), s(10), s(11), s(12), s(13), s(14))
    case 16 => new Tuple16(s(0), s(1), s(2), s(3), s(4), s(5), s(6), s(7), s(8), s(9), s(10), s(11), s(12), s(13), s(14), s(15))
    case 17 => new Tuple17(s(0), s(1), s(2), s(3), s(4), s(5), s(6), s(7), s(8), s(9), s(10), s(11), s(12), s(13), s(14), s(15), s(16))
    case 18 => new Tuple18(s(0), s(1), s(2), s(3), s(4), s(5), s(6), s(7), s(8), s(9), s(10), s(11), s(12), s(13), s(14), s(15), s(16), s(17))
    case 19 => new Tuple19(s(0), s(1), s(2), s(3), s(4), s(5), s(6), s(7), s(8), s(9), s(10), s(11), s(12), s(13), s(14), s(15), s(16), s(17), s(18))
    case 20 => new Tuple20(s(0), s(1), s(2), s(3), s(4), s(5), s(6), s(7), s(8), s(9), s(10), s(11), s(12), s(13), s(14), s(15), s(16), s(17), s(18), s(19))
    case 21 => new Tuple21(s(0), s(1), s(2), s(3), s(4), s(5), s(6), s(7), s(8), s(9), s(10), s(11), s(12), s(13), s(14), s(15), s(16), s(17), s(18), s(19), s(20))
    case 22 => new Tuple22(s(0), s(1), s(2), s(3), s(4), s(5), s(6), s(7), s(8), s(9), s(10), s(11), s(12), s(13), s(14), s(15), s(16), s(17), s(18), s(19), s(20), s(21))
    case _ => new ProductWrapper(s)
  }

  /** Build an IndexedSeq from a Product */
  def buildIndexedSeq(p: Product): IndexedSeq[Any] = p match {
    case p: ProductWrapper => p.seq
    case p => p.productIterator.toIndexedSeq
  }
}
/** A Product to represent larger arities than Tuple22 */
final class ProductWrapper(val seq: IndexedSeq[Any]) extends Product {
  def productArity = seq.length
  def productElement(idx: Int) = seq(idx)
  override def productIterator = seq.iterator
  def canEqual(that: Any) = that.isInstanceOf[ProductWrapper]
  override def equals(that: Any) = that match {
    case p: ProductWrapper => productArity == p.productArity &&
      (0 until productArity).forall(i => productElement(i) == p.productElement(i))
    case _ => false
  }
  override def hashCode = seq.hashCode
  override def toString = seq.mkString("ProductWrapper(", ",", ")")
}
