package odo.suffixarr

import java.util

import spire.syntax.cfor._

import scala.collection.mutable

abstract class SuffixArray[+C] {
  def original: IndexedSeq[C]
  def suffixArray: Array[Int]
  def LCP: Array[Int]
  override def toString = suffixArray.mkString("SuffixArray[", ",", "]")
}

object SuffixArray {

  trait Maker[-C] {
    def apply[C1 <: C : BoundEnum](seq: IndexedSeq[C1]): SuffixArray[C1]
    def forElems[C1 <: C : Ordering](seq: IndexedSeq[C1]): SuffixArray[C1] = {
      implicit val derivedBounds = BoundEnum.forElems(seq)
      apply(seq)
    }
  }

  def apply[T: BoundEnum](seq: IndexedSeq[T])(implicit maker: Maker[T] = NaiveMaker): SuffixArray[T] = maker(seq)

  object NaiveMaker extends Maker[Any] {
    def lcp[T](as: IndexedSeq[T], bs: IndexedSeq[T])(implicit O: Ordering[T]): Int = as.zip(bs).takeWhile((O.equiv _).tupled).size

    def apply[T: BoundEnum](seq: IndexedSeq[T]): SuffixArray[T] = new SuffixArray[T] {
      val original: IndexedSeq[T] = seq
      val suffixArray = seq.tails.map(_.toIterable).toArray.sorted.map(seq.size - _.size)
      val LCP = suffixArray.iterator.zip(suffixArray.tail.iterator).map { case (x, y) ⇒ lcp(seq.drop(x), seq.drop(y)) }.toArray
    }
  }

  def lcpFromSA[A](seq: IndexedSeq[A], arr: Array[Int]): Array[Int] = {
    val inverse = Array.ofDim[Int](arr.length)
    val lcp = Array.ofDim[Int](seq.length)
    cforRange(0 until arr.length) { i ⇒ inverse(arr(i)) = i }
    var l = 0
    val n = seq.size
    cforRange(0 until arr.length) { i ⇒
      var k = inverse(i)
      if (k > 0) {
        var j = arr(k - 1)
        while ((j + l) < seq.size && seq(i + l) == seq(j + l)) l += 1
        lcp(k - 1) = l
        if (l > 0) l -= 1
      }
    }
    lcp
  }
}

object SA_ISMaker extends SuffixArray.Maker[Any] {
  def apply[T: BoundEnum](seq: IndexedSeq[T]): SuffixArray[T] = new SA_ISMaker(seq).build
}

class SA_ISMaker[C](val seq: IndexedSeq[C])(implicit bounds: BoundEnum[C]) {
  val n = seq.size
  var summary: Summary = null

  val typeMap: mutable.BitSet = {
    val bitSet = new mutable.BitSet(seq.size + 1)
    if (seq.nonEmpty) bitSet add (n - 1)
    cforRange((n - 2) to 0 by -1) { i ⇒
      bounds.compare(seq(i), seq(i + 1)) match {
        case res if res > 0 ⇒ bitSet add i
        case 0 if bitSet(i + 1) ⇒ bitSet add i
        case _ ⇒
      }
    }
    bitSet
  }

  def isLMS(i: Int): Boolean = i != 0 && !typeMap(i) && typeMap(i - 1)

  def showTypeMap = (0 to n).map(i ⇒ if (typeMap(i)) "L" else if (isLMS(i)) "$" else "S").mkString

  private def lmsSubsEqIter(i: Int, j: Int): Boolean = {
    val lmsI = isLMS(i)
    val lmsJ = isLMS(j)
    if (lmsI && lmsJ) true
    else if (lmsI != lmsJ) false
    else if (seq(i) != seq(j)) false
    else lmsSubsEqIter(i + 1, j + 1)
  }

  def lmsSubsEqual(i: Int, j: Int): Boolean =
    i != n && j != n && seq(i) == seq(j) && lmsSubsEqIter(i + 1, j + 1)

  val bucketSizes: Array[Int] = {
    val sizes = Array.ofDim[Int](bounds.size)
    seq.foreach(c ⇒ sizes(bounds.toInt(c)) += 1)
    sizes
  }

  def bucketHeads = bucketSizes.scanLeft(1)(_ + _).init
  def bucketTails = bucketSizes.scanLeft(0)(_ + _).tail

  def start = ProducedArray(Array.fill(n + 1)(-1))

  def build: SuffixArray[C] =
    new SuffixArray[C] {
      val original = seq
      val suffixArray = start.complete.arr
      val LCP = SuffixArray.lcpFromSA(seq, suffixArray)
    }

  case class ProducedArray(arr: Array[Int]) {
    def guessLMSSort: this.type = {
      val bt = bucketTails
      cforRange(0 until n) { i ⇒
        if (isLMS(i)) {
          val idx = bounds.toInt(seq(i))
          arr(bt(idx)) = i
          bt(idx) -= 1
        }
      }
      arr(0) = n
      this
    }

    def printAt(i: Int = -1): Unit = {
      println(arr.zipWithIndex.map {
        case (e, `i`) => "[" + e + "]"
        case (e, _) => e
      }.mkString(", "))
    }

    def induceSortL: this.type = {
      val bh = bucketHeads

      cforRange(0 to n) { i ⇒
        if (arr(i) >= 0) {
          val j = arr(i) - 1

          if (j >= 0 && typeMap(j)) {
            val bIndex = bounds.toInt(seq(j))
            arr(bh(bIndex)) = j
            bh(bIndex) += 1
          }
        }
      }
      this
    }

    def induceSortS: this.type = {
      val bt = bucketTails

      cforRange(n to 0 by -1) { i =>
        val j = arr(i) - 1
        if (j >= 0 && !typeMap(j)) {
          val bIndex = bounds.toInt(seq(j))
          arr(bt(bIndex)) = j
          bt(bIndex) -= 1
        }
      }
      this
    }

    def summarize: this.type = {
      val lmsNames = Array.fill(n + 1)(-1)
      var currentName = 0
      lmsNames(arr(0)) = currentName
      var last: Int = arr(0)
      var count = 1

      cforRange(1 to n) { i =>
        val current = arr(i)
        if (isLMS(current)) {
          if (!lmsSubsEqual(current, last)) currentName += 1
          last = current
          lmsNames(current) = currentName
          count += 1
        }
      }

      val size = currentName + 1
      val offsets = Array.ofDim[Int](count)
      val names = Array.ofDim[Int](count)

      lmsNames.iterator.zipWithIndex.filter(_._1 != -1).zipWithIndex.foreach {
        case ((name, offset), idx) =>
          offsets(idx) = offset
          names(idx) = name
      }

      summary = Summary(names, size, offsets)
      this
    }

    def accurateLMSSort: this.type = {
      val bt = bucketTails
      val sumArr = summary.suffixArray
      val sumOff = summary.offsets
      util.Arrays.fill(arr, -1)
      cforRange((sumArr.length - 1) to 2 by -1) { i =>
        val idx = sumOff(sumArr(i))
        val bIndex = bounds.toInt(seq(idx))
        arr(bt(bIndex)) = idx
        bt(bIndex) -= 1
      }
      arr(0) = n

      this
    }

    def complete = this
                   .guessLMSSort
                   .induceSortL
                   .induceSortS
                   .summarize
                   .accurateLMSSort
                   .induceSortL
                   .induceSortS

    override def toString = arr.mkString(s"Produced($seq)[", ", ", "]")
  }

  case class Summary(names: Array[Int], size: Int, offsets: Array[Int]) {
    def bucketSort: Array[Int] = {
      val result = Array.ofDim[Int](offsets.length + 1)
      result(0) = names.length
      cforRange(0 until names.size) { i =>
        result(names(i) + 1) = i
      }
      result
    }

    def induction: Array[Int] = new SA_ISMaker(names)(BoundEnum.intRange(0, size - 1)).start.complete.arr

    def suffixArray = if (names.length == size) bucketSort else induction

    override def toString = s"Summary(names = ${names.mkString("[", ", ", "]")}; size = $size; offsets= ${offsets.mkString("[", ", ", "]")})"
  }
}
