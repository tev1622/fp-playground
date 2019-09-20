package scalawithcats.functors

trait Codec[A] {
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B]
}

object Codec {
  def encode
}