package slick.test

import slick.ast.SymbolNamer
import slick.util.TreePrinter
import slick.lifted._
import slick.lifted.API._
import slick.collection.heterogeneous._

object LiftedEmbeddingTest extends App {

  case class User(id: Int, first: String, last: String)

  class Users(tag: Tag) extends Table[(Int, String, Option[String])](tag, "users") {
    def id = column[Int]("id")
    def first = column[String]("first")
    def last = column[Option[String]]("last")
    def * = (id, first, last)
  }
  lazy val users = TableQuery(new Users(_))

  class Orders(tag: Tag) extends Table[(Int, Int, String, Boolean, Option[Boolean])](tag, "orders") {
    def userID = column[Int]("userID")
    def orderID = column[Int]("orderID")
    def product = column[String]("product")
    def shipped = column[Boolean]("shipped")
    def rebate = column[Option[Boolean]]("rebate")
    def * = (userID, orderID, product, shipped, rebate)
  }
  lazy val orders = TableQuery(new Orders(_))

  def run[T](r: Rep[T]): T = {
    (new SymbolNamer("s", "t", None)).use(TreePrinter.default.print(r.toNode))
    null.asInstanceOf[T]
  }

  val q1 = (for(u <- users) yield (u.id, u.first, u.last))
  run(q1)

  val q2 = (for(u <- users) yield (u.id, (u.first, u.last)))
  run(q2)
}
