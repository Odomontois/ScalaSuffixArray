package odo.suffixarr
import scala.util.Random

object Test {
  def randString = Random.nextString(100)

  class Bench(name: String) {
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

  def main(args: Array[String]): Unit = {
    //    val string = "rikki-tikki-tikka"
    val issaBench = new Bench("SA-IS")
//    val naiveBench = new Bench("naive")
    for (i ← 1 to 20) {
      println(i)

      val string = Random.nextString(30000)
      implicit val char = BoundEnum.forElems(string)
//      val naive = naiveBench.bench(SuffixArray.NaiveMaker(string))
      val issa = issaBench.bench(SA_ISMaker(string))
//      println(issa.suffixArray == naive.suffixArray)
    }

//    println(naiveBench.stats)
    println(issaBench.stats)
  }
}
