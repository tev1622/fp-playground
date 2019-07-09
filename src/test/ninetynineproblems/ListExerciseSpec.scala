package ninetynineproblems

import ninetynineproblems.ListExercise._
import org.scalatest.{FlatSpec, Matchers}

class ListExerciseSpec extends FlatSpec with Matchers {

  private val list = List(1, 2, 3, 4, 5)

  "last" should "return the last element in the list" in {
    last(list) shouldBe 5
  }

  "penultimate" should "return the penultimate element in the list" in {
    penultimate(list) shouldBe 4
  }

  "nth" should "return the 2nd element in the list" in {
    nth(1, list) shouldBe 2
  }

  "length" should "return the number of elements of a list" in {
    ListExercise.length(list) shouldBe 5
  }

  "reverse" should "reverse the elements in a list" in {
    reverse(list) shouldBe List(5, 4, 3, 2, 1)
  }

  "isPalindrome" should "return true if the list is a palindrome" in {
    val l = List(1, 2, 3, 2, 1)
    isPalindrome(l) shouldBe true
  }

  it should "return false if the list isn't a palindrome" in {
    isPalindrome(list) shouldBe false
  }

  "flatten" should "flatten a nested structure list" in {
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) shouldBe List(1, 1, 2, 3, 5, 8)
  }

  "compress" should "Eliminate consecutive duplicates of list elements" in {
    compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List('a, 'b, 'c, 'a, 'd, 'e)
  }

  "pack" should "Pack consecutive duplicates of list elements into sublists" in {
    pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  }

  "encode" should "Run-length encoding of a list" in {
    encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  }

  "encodeModified" should "no duplicates it is simply copied into the result list" in {
    encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  }

  "decode" should "Decode a run-length encoded list" in {
    decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) shouldBe List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  }

  "encodeDirect" should "Run-length encoding of a list " in {
    encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  }

  "duplicate" should "Duplicate the elements of a list" in {
    duplicate(List('a, 'b, 'c, 'c, 'd)) shouldBe List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  }


  "duplicateN" should "" in {
    duplicateN(3, List('a, 'b, 'c, 'c, 'd)) shouldBe List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  }

  "drop" should "drop every nth element in a list" in {
    drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldBe List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  }

  "split" should "Split a list into two parts" in {
    split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldBe (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  }

  "slice" should "slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list" in {
    slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldBe List('d, 'e, 'f, 'g)
  }

}
