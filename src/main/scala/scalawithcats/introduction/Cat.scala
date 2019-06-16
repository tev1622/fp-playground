package scalawithcats.introduction

import cats.{Eq, Show}
import cats.syntax.show._
import cats.syntax.eq._
import cats.instances.int._
import cats.instances.string._
import cats.instances.option._

final case class Cat(name: String, age: Int, color: String)

object Cat extends App {
//  import PrintableInstances._
//  import PrintableSyntax._
//

  implicit val catShow: Show[Cat] = Show.show(c => s"${c.name} is a ${c.age} year-old ${c.color} cat.")
  val cat = Cat("tommy", 12, "red")//.print

  implicit val catEq: Eq[Cat] =
    Eq.instance[Cat] { (c, cc) =>
      (c.name === cc.name) && (c.age === cc.age) && (c.color === cc.color)
    }

  val cat1 = Cat("Garfield",   38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")

  cat1 === cat2

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  optionCat1 === optionCat2
}
