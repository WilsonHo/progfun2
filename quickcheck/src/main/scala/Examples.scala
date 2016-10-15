/**
  * Created by baohg on 13/10/2016.
  */

import StringSpecification._
import org.scalacheck.{Gen, Prop, Properties}
import org.scalacheck.Prop.{forAll, BooleanOperators}

object StringSpecification

object Examples extends Properties("String") {

  property("startsWith") = forAll { (a: String, b: String) =>
    (a + b).startsWith(a)
  }

  property("concatenate") = forAll { (a: String, b: String) =>
    (a + b).length > a.length && (a + b).length > b.length
  }

  property("substring") = forAll { (a: String, b: String, c: String) =>
    (a + b + c).substring(a.length, a.length + b.length) == b
  }

  property("myprop") = forAll { l: List[Int] =>
    l.reverse.reverse == l
  }

  property("propConcatLists") = forAll { (l1: List[Int], l2: List[Int]) =>
    l1.size + l2.size == (l1 ::: l2).size
  }

  property("propSqrt") = forAll { (n: Int) =>
    scala.math.sqrt(n * n) == n
  }

  property("propConcatString") = forAll { (s1: String, s2: String) =>
    (s1 + s2).endsWith(s2)
  }

  val smallInteger = Gen.choose(-3, 101)
  property("propSmallInteger") = forAll(smallInteger) { n =>
    n >= 0 && n <= 100
  }

  property("propMakeList") = forAll { n: Int =>
    (n >= 0 && n < 100) ==> (List.fill(n)("").length == n)
  }

  property("propTrivial") = forAll { n: Int =>
    (n == 0) ==> (n == 0)
  }

  def myMagicFunction(n: Int, m: Int) = n + m
  property("complexProp") = forAll { (m: Int, n: Int) =>
    val res = myMagicFunction(n, m)
    (res >= m) :| "result > #1" &&
      (res >= n) :| "result > #2" &&
      (res < m + n) :| "result not sum"
  }
}