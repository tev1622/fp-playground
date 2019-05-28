package scalawithcats.introduction

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
  }