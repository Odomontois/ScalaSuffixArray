package odo.suffixarr
import java.util

import scala.util.{Random, Try}

object Test {
  def randString = Random.nextString(100)

  abstract class Bench(val name: String) {
    def method(str: String)(implicit be: BoundEnum[Char]): SuffixArray[Char]

    def run(str: String)(implicit be: BoundEnum[Char]) = bench(method(str))

    private var measures: Vector[Double] = Vector.empty
    def bench[A](actions: ⇒ A): A = {
      val start = System.nanoTime()
      val res = actions
      measures :+= (System.nanoTime - start) / 1e6
      res
    }

    def average = measures.sum.toDouble / measures.size
    def medium = {
      val srt = measures.sorted
      val idx = (measures.size + 1) / 2
      if (measures.size % 2 == 1) srt(idx) else (srt(idx) + srt(idx + 1)) / 2
    }

    def stats = s"$name avg: $average ; mid: $medium"
  }

  val benchMap = Seq(
    new Bench("SA-IS") {
      def method(str: String)(implicit be: BoundEnum[Char]): SuffixArray[Char] = SA_ISMaker(str)
    },
    new Bench("naive") {
      def method(str: String)(implicit be: BoundEnum[Char]): SuffixArray[Char] = SuffixArray.NaiveMaker(str)
    }
  ).map(b ⇒ b.name → b).toMap

  def bench(size: Int, count: Int, benchNames: Seq[String]): Unit = {
    val benchs = benchNames.map(benchMap)
    for (i ← 1 to count) {
      println(i)

      val string = Random.nextString(size)
      implicit val char = BoundEnum.forElems(string)
      benchs.foreach(_.run(string))
    }
    benchs.foreach(b ⇒ println(b.stats))
  }

  def compare(str: String, be: Option[BoundEnum[Char]] = None): Unit = {
    println(str)
    implicit val bounds = be getOrElse BoundEnum.forElems(str)
    val issa = SA_ISMaker(str)
    val naive = SuffixArray.NaiveMaker(str)
    assert(util.Arrays.equals(issa.suffixArray, naive.suffixArray), "suffix arrays distincts")
    assert(util.Arrays.equals(issa.LCP, naive.LCP), "LCP distincts")
  }

  def main(args: Array[String]): Unit = args match {
    case Array(IntString(size), IntString(count), names@_*) ⇒ bench(size, count, names)
    case Array(IntString(size)) ⇒ compare(Random.nextString(size))
    case Array(IntString(size), "ASCII") ⇒ compare(
      str = Array.fill(size)(Random.nextInt(117) + 10).iterator.map(_.toChar).mkString,
      be = Some(BoundEnum.ascii))
    case Array(string) ⇒ compare(string)
  }

  object IntString {
    def unapply(arg: String): Option[Int] = Try(arg.toInt).toOption
  }
}
