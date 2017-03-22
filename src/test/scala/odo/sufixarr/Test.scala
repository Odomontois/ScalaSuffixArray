package odo.suffixarr
import java.util
import java.util.regex.Pattern

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

    def average = measures.sum / measures.size
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

  def bench(count: Int, benchNames: Seq[String])(make: ⇒ String): Unit = {
    val benchs = benchNames.map(benchMap)
    for (i ← 1 to count) {
      println(i)

      val string = make
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

  def match_in(string: String, pattern: String): Unit = {
    val arr = SuffixArray(string)
    println(arr.searchAny(pattern).fold("NOT FOUND")(i ⇒ s"$i : ${string.drop(i)}"))
  }

  object Options {
    def unapply(arg: String): Option[Set[String]] = Some(arg.substring(1, arg.length - 1).split(",").map(_.trim.toLowerCase).toSet)
  }
  object Command {
    val cmd = "(\\w+)(\\[.*\\])?".r
    def nextAscii(size: Int) = Iterator.fill(size)(Random.nextInt(117) + 10).map(_.toChar).mkString

    def unapply(arg: String): Option[(String, Int ⇒ String)] = arg match {
      case cmd(name, null) ⇒ Some(name, size ⇒ Random.nextString(size))
      case cmd(name, Options(opts)) if opts("ascii") ⇒ Some(name, size ⇒ nextAscii(size))
    }
  }

  def main(args: Array[String]): Unit = args match {
    case Array(Command("BENCH", rnd), IntString(size), IntString(count), names@_*) ⇒ bench(count, names)(rnd(size))
    case Array(Command("CYCLE", rnd), IntString(size), IntString(repeats), IntString(count), names@_*) ⇒ bench(count, names)(rnd(size) * repeats)
    case Array(Command("CYCLECHECK", rnd), IntString(size), IntString(repeats)) ⇒ compare(rnd(size) * repeats)
    case Array(Command("CHECK", rnd), IntString(size)) ⇒ compare(rnd(size))
    case Array(Command("CHECK", rnd), IntString(size), IntString(repeat)) ⇒ compare(rnd(size) * repeat)
    case Array(Command("CHECK", _), string) ⇒ compare(string)
    case Array("MATCH", string, pattern) ⇒ match_in(string, pattern)
  }

  object IntString {
    def unapply(arg: String): Option[Int] = Try(arg.toInt).toOption
  }
}
