package scalawithcats.introduction
import cats.instances.all._
trait Printable[A] {
  def format(value: A): String
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
  }

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit printable: Printable[A]): String = printable.format(value)
    def print(implicit printable: Printable[A]): Unit = println(format)
  }
}