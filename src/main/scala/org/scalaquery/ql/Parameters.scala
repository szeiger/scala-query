package org.scalaquery.ql

import org.scalaquery.SQueryException
import org.scalaquery.ql.basic.{BasicProfile, BasicQueryTemplate}
import org.scalaquery.util.{TupledEvidence, NaturalTransformation1}

final class Parameters[P, C](c: C) {
  def flatMap[F](f: C => Query[_, F])(implicit profile: BasicProfile): BasicQueryTemplate[P, F] =
    profile.createQueryTemplate[P, F](f(c))

  def map[F](f: C => ColumnBase[F])(implicit profile: BasicProfile): BasicQueryTemplate[P, F] =
    profile.createQueryTemplate[P, F](Query(f(c)))

  def filter(f: C => Boolean): Parameters[P, C] =
    if (!f(c)) throw new SQueryException("Match failed when unpacking Parameters")
    else this

  def withFilter(f: C => Boolean) = filter(f)
}

object Parameters {
  def apply[T](implicit w: TupledEvidence[TypeMapper, T]): Parameters[T, w.Map[ParameterColumn]] = {
    var idx = if(w.length == 1) -2 else -1
    new Parameters[T, w.Map[ParameterColumn]](
      w.map(new NaturalTransformation1[TypeMapper, ParameterColumn] {
        def apply[T](t: TypeMapper[T]) = { idx += 1; new ParameterColumn[T](idx)(t) }
      }))
  }
}
