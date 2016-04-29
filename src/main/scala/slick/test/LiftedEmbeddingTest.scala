package slick.test

import slick.util.TreePrinter
import slick.lifted._
import slick.collection.heterogeneous.HNil
import slick.collection.heterogeneous.syntax._

object LiftedEmbeddingTest extends App {

  def run[T](r: Rep[T]): T = {
    TreePrinter(r.toNode)
    null.asInstanceOf[T]
  }

  def test1 {
    class Users(tag: Tag) extends Table[(Int, String, String)](tag, "users") {
      def id = column[Int]("id")
      def first = column[String]("first")
      def last = column[String]("last")
      def * = (id, first, last)
    }
    lazy val users = TableQuery(new Users(_))

    val q = users.filter(_.id < 42).map(_.first)
    run(q)

    val q1 = users.filter(_.id < 42).map(u => (u.id, u.first, u.last))
    run(q1)

    val q2 = (for(u <- users) yield (u.id, (u.first, u.last)))
    run(q2)

    val q3 = users.map(u => (u.first, 42)).map(t => (t._1, t._2))
    run(q3)

    val q4 = users.nmap(u => (u.first, 42)) //.nmap(t => (t._1, t._2))
    run(q4)

    val q5 = users.filter(u => u.id < 42)
    run(q5)
  }

  def test2 { // MappedProjection
    case class User(id: Int, first: String, last: String)
    class Users(tag: Tag) extends Table[User](tag, "users") {
      def id = column[Int]("id")
      def first = column[String]("first")
      def last = column[String]("last")
      def * = (id, first, last) <> (User.tupled, User.unapply _)
    }
    lazy val users = TableQuery(new Users(_))

    /*class Orders(tag: Tag) extends Table[(Int, Int, String, Boolean)](tag, "orders") {
      def userID = column[Int]("userID")
      def orderID = column[Int]("orderID")
      def product = column[String]("product")
      def shipped = column[Boolean]("shipped")
      def * = (userID, orderID, product, shipped)
    }
    lazy val orders = TableQuery(new Orders(_))*/

    val q1 = users.filter(_.id < 42)
    val r1: Seq[User] = run(q1)
  }

  def test3 { // CaseClassShape
    case class User(id: Int, first: String, last: String)
    case class LiftedUser(id: Rep[Int], first: Rep[String], last: Rep[String])
    implicit object userShape extends CaseClassShape(LiftedUser.tupled, User.tupled)

    class Users(tag: Tag) extends Table[User](tag, "users") {
      def id = column[Int]("id")
      def first = column[String]("first")
      def last = column[String]("last")
      def all = LiftedUser(id, first, last)
      def * = all
    }
    lazy val users = TableQuery(new Users(_))

    val q1 = users.filter(_.id < 42).map(_.all)
    val r1: Seq[User] = run(q1)
  }

  def test4 { // HList

    class Users(tag: Tag) extends Table[Int :: String :: String :: HNil](tag, "users") {
      def id = column[Int]("id")
      def first = column[String]("first")
      def last = column[String]("last")
      def * = id :: first :: last :: HNil
    }
    lazy val users = TableQuery(new Users(_))

    val q1 = users.map(u => u.id :: u.first :: HNil)
    val r1: Seq[Int :: String :: HNil] = run(q1)
  }

  test1
}
