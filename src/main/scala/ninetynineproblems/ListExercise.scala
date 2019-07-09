package ninetynineproblems

object ListExercise {

  def last[A](list: List[A]): A = list match {
    case h :: Nil => h
    case _ :: t => last(t)
    case _ => throw new NoSuchElementException
  }

  def penultimate[A](list: List[A]): A = list match {
    case h :: _ :: Nil => h
    case _ :: tail => penultimate(tail)
    case _ => throw new NoSuchElementException
  }

  def nth[A](index: Int, list: List[A]): A =
    list(index)

  def length[A](list: List[A]): Int =
    list.foldLeft(0)((acc, _) => acc + 1)

  def reverse[A](list: List[A]): List[A] =
    list.foldLeft(List[A]())((a, l) => l :: a)

  def isPalindrome[A](list: List[A]): Boolean =
    list == reverse(list)

  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }

  def compress[A](list: List[A]): List[A] = list.foldRight(List[A]()) {
    (a, al) =>
      if (al.isEmpty || al.head != a) a :: al
      else al
  }

  def pack[A](l: List[A]): List[List[A]] = {
    if (l.isEmpty) List[List[A]]()
    val (collectedList, nextList) = l.span(_ == l.head)
    if (nextList.isEmpty) List(collectedList) else
      collectedList :: pack(nextList)
  }

  def encode[A](l: List[A]): List[(Int, A)] =
    pack(l).map(a => (a.length, a.head))

  def encodeModified[A](l: List[A]): List[Any] =
    encode(l).map {
      case (i, a) if i == 1 => a
      case x => x
    }

  def decode[A](l: List[(Int, A)]): List[A] =
    l.flatMap(i => List.fill(i._1)(i._2))

  def encodeDirect[A](l: List[A]): List[(Int, A)] = {
    if (l.isEmpty) Nil else {
      val (cl, nl) = l.span(_ == l.head)
      (cl.size, cl.head) :: encodeDirect(nl)
    }
  }

  def duplicate[A](l: List[A]): List[A] =
    l.foldLeft(List[A]())((acc, a) => acc :+ a :+ a)

  def duplicateN[A](n: Int, l: List[A]): List[A] =
    l.flatMap(List.fill(n)(_))

  def drop[A](n: Int, l: List[A]): List[A] =
    l.zipWithIndex.filterNot(a => (a._2 + 1) % n == 0).map(_._1)

  def split[A](n: Int, list: List[A]):(List[A], List[A]) =
    (list.take(3), list.drop(3))

  def slice[A](i: Int, k: Int, list: List[A]): List[A] =
    list.drop(i).take(k - i)
}
