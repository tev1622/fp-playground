package scalawithcats.introduction
import cats.instances.all._
import scalawithcats.functors.Box
trait Printable[A] { self =>
  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] = new Printable[B] {
    def format(value: B): String = self.format(func(value))
  }
}

object Printable {
  def format[A](value: A)(implicit printable: Printable[A]): String =
    printable.format(value)

  def print[A](value: A)(implicit printable: Printable[A]): Unit =
    println(printable.format(value))
}

  object PrintableInstances {
    implicit val printableString: Printable[String] = new Printable[String] {
      override def format(value: String): String = value
    }

    implicit val printableInt: Printable[Int] = new Printable[Int] {
      override def format(value: Int): String = value.toString
    }

    implicit val printableCat: Printable[Cat] = new Printable[Cat] {
      override def format(value: Cat): String = {
        val name = Printable.format(value.name)
        val age = Printable.format(value.age)
        val color = Printable.format(value.color)

        s"$name is a $age year-old $color cat."
      }
    }

    implicit def printableBox[A](implicit p: Printable[A]): Printable[Box[A]] =
      p.contramap[Box[A]](_.value)
  }

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit printable: Printable[A]): String = printable.format(value)
    def print(implicit printable: Printable[A]): Unit = println(format)
  }
}