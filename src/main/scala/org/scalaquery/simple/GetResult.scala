package org.scalaquery.simple

import java.sql.{Date, Time, Timestamp}
import org.scalaquery.SQueryException
import org.scalaquery.session.PositionedResult
import org.scalaquery.util.{CovariantTupledEvidence, TupledEvidence, NaturalTransformation, NaturalTransformation1}

/**
 * Basic conversions for extracting values from PositionedResults.
 */
trait GetResult[+T] extends (PositionedResult => T) { self =>
  override def andThen[A](g: T => A): GetResult[A] = new GetResult[A] { def apply(rs: PositionedResult): A = g(self.apply(rs)) }
}

object GetResult {
  implicit object GetBoolean extends GetResult[Boolean] { def apply(rs: PositionedResult) = rs.nextBoolean() }
  implicit object GetByte extends GetResult[Byte] { def apply(rs: PositionedResult) = rs.nextByte() }
  implicit object GetDate extends GetResult[Date] { def apply(rs: PositionedResult) = rs.nextDate() }
  implicit object GetDouble extends GetResult[Double] { def apply(rs: PositionedResult) = rs.nextDouble() }
  implicit object GetFloat extends GetResult[Float] { def apply(rs: PositionedResult) = rs.nextFloat() }
  implicit object GetInt extends GetResult[Int] { def apply(rs: PositionedResult) = rs.nextInt() }
  implicit object GetLong extends GetResult[Long] { def apply(rs: PositionedResult) = rs.nextLong() }
  implicit object GetShort extends GetResult[Short] { def apply(rs: PositionedResult) = rs.nextShort() }
  implicit object GetString extends GetResult[String] { def apply(rs: PositionedResult) = rs.nextString() }
  implicit object GetTime extends GetResult[Time] { def apply(rs: PositionedResult) = rs.nextTime() }
  implicit object GetTimestamp extends GetResult[Timestamp] { def apply(rs: PositionedResult) = rs.nextTimestamp() }

  implicit object GetBooleanOption extends GetResult[Option[Boolean]] { def apply(rs: PositionedResult) = rs.nextBooleanOption() }
  implicit object GetByteOption extends GetResult[Option[Byte]] { def apply(rs: PositionedResult) = rs.nextByteOption() }
  implicit object GetDateOption extends GetResult[Option[Date]] { def apply(rs: PositionedResult) = rs.nextDateOption() }
  implicit object GetDoubleOption extends GetResult[Option[Double]] { def apply(rs: PositionedResult) = rs.nextDoubleOption() }
  implicit object GetFloatOption extends GetResult[Option[Float]] { def apply(rs: PositionedResult) = rs.nextFloatOption() }
  implicit object GetIntOption extends GetResult[Option[Int]] { def apply(rs: PositionedResult) = rs.nextIntOption() }
  implicit object GetLongOption extends GetResult[Option[Long]] { def apply(rs: PositionedResult) = rs.nextLongOption() }
  implicit object GetShortOption extends GetResult[Option[Short]] { def apply(rs: PositionedResult) = rs.nextShortOption() }
  implicit object GetStringOption extends GetResult[Option[String]] { def apply(rs: PositionedResult) = rs.nextStringOption() }
  implicit object GetTimeOption extends GetResult[Option[Time]] { def apply(rs: PositionedResult) = rs.nextTimeOption() }
  implicit object GetTimestampOption extends GetResult[Option[Timestamp]] { def apply(rs: PositionedResult) = rs.nextTimestampOption() }

  implicit def createGetTuple[T <: Product](implicit w: CovariantTupledEvidence[GetResult, T]): GetResult[T] = (new GetResult[w.Map[NaturalTransformation.IdentityFunctor]] {
    def apply(rs: PositionedResult) =
      w.map(new NaturalTransformation1[GetResult, NaturalTransformation.IdentityFunctor] {
        def apply[T](t: GetResult[T]) = t(rs)
      })
  }).asInstanceOf[GetResult[T]]

  private[simple] object GetUpdateValue extends GetResult[Int] {
    def apply(pr: PositionedResult) =
      throw new SQueryException("Update statements should not return a ResultSet")
  }

  def apply[T](implicit f: (PositionedResult => T)) = new GetResult[T] { def apply(rs: PositionedResult) = f(rs) }
}
