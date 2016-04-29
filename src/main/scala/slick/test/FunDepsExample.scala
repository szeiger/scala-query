package slick.test

object FunDepsExample {

  class Convert[From, To](val f: From => To)
  object Convert {
    implicit val intToLong    = new Convert[Int,    Long  ](_.toLong)
    implicit val longToString = new Convert[Long,   String](_.toString)
    implicit val stringToInt  = new Convert[String, Int   ](_.toInt)
  }

  def f[T1, T2](v: T1)(implicit conv: Convert[T1, T2]): T2 = conv.f(v)

  val l          = f(42)
  val s          = f(l)
  val i          = f(s)

  /*
  val l: Long    = f(42)
  val s: String  = f(l)
  val i: Int     = f(s)
  */
}
