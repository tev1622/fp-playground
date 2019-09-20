package scalawithcats.functors

import cats.Functor
import cats.instances.function._
import cats.syntax.functor._
import scalawithcats.introduction.Printable // for map

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object Tree {

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      case Leaf(a) => Leaf(f(a))
    }
  }
}

final case class Box[A](value: A)

object FunctorFun extends App {
  import Tree._

  branch(Leaf(10), Leaf(20)).map(_ * 2)

}