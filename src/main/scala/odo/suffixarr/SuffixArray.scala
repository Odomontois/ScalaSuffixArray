package odo.suffixarr

import spire.syntax.cfor._

trait SuffixArray[C] {
  def original: IndexedSeq[C]
  def suffixArray: Array[Int]
  def LCP: Array[Int]
  def LCP_LR: Array[Int]
  def bound: BoundEnum[C]

  def searchAny(word: IndexedSeq[C]): Option[Int] = {
    var grow = false
    val n = original.length
    val m = word.length

    def compareTo(num: Int, start: Int): Int = {
      var i = suffixArray(num) + start
      var result = start
      while (i < n && result < m && original(i) == word(result)) {
        i += 1
        result += 1
      }
      if (i < n && result < m && bound.lt(original(i), word(result)))
        grow = true
      else grow = false
      result
    }

    def go(from: Int, to: Int, matched: Int, idx: Int): Option[Int] = if (to - from == 1)
      if (LCP(to) != matched) None
      else if (compareTo(to, matched) != word.length) None
      else Some(suffixArray(to))
    else {
      val newLCP = if (to - from == 2)
        if (grow) LCP(from + 1) else LCP(from)
      else if (grow) LCP_LR(idx * 2 + 1) else LCP_LR(idx * 2 + 2)

      val mid = (from + to) / 2
      if (newLCP > matched)
        if (grow) go(mid, to, matched, idx * 2 + 2)
        else go(from, mid, matched, idx * 2 + 1)
      else if (newLCP < matched)
        if (grow) go(from, mid, newLCP, idx * 2 + 1)
        else go(mid, to, newLCP, idx * 2 + 2)
      else {
        val newMatch = compareTo(mid, matched)
        if (newMatch == word.length) Some(suffixArray(mid))
        else if (grow) go(mid, to, newMatch, idx * 2 + 2)
        else go(from, mid, newMatch, idx * 2 + 1)
      }
    }

    go(0, LCP.length, 0, 0)
  }
}

private[suffixarr] class SuffixArrayImpl[C](val original: IndexedSeq[C],
                                            val suffixArray: Array[Int],
                                            val LCP: Array[Int]
                                           )(implicit val bound: BoundEnum[C]) extends SuffixArray[C] {
  val LCP_LR = SuffixArray.buildLCP_LR(LCP)
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

  def apply[T: BoundEnum](seq: IndexedSeq[T])(implicit maker: Maker[T] = SA_ISMaker): SuffixArray[T] = maker(seq)

  object NaiveMaker extends Maker[Any] {
    def lcp[T](as: IndexedSeq[T], bs: IndexedSeq[T])(implicit O: Ordering[T]): Int = as.zip(bs).takeWhile((O.equiv _).tupled).size

    def apply[T: BoundEnum](seq: IndexedSeq[T]): SuffixArray[T] = {
      val arr = seq.tails.map(_.toIterable).toArray.sorted.map(seq.size - _.size)
      val lcp_arr = arr.iterator.zip(arr.tail.iterator).map { case (x, y) ⇒ lcp(seq.drop(x), seq.drop(y)) }.toArray
      new SuffixArrayImpl[T](seq, arr, lcp_arr)
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

  def calcLCP_LR_size(original: Int): Int = {
    def iter(num: Int): Int = {
      val next = num & (num - 1)
      if (next == 0) num * 2 - 1 else iter(next)
    }
    iter(original)
  }

  def buildLCP_LR(lcp: Array[Int]): Array[Int] = {
    val arr = Array.ofDim[Int](calcLCP_LR_size(lcp.length))
    def go(from: Int, to: Int, idx: Int): Int =
      if (to - from == 1) lcp(from) else {
        val mid = (from + to) / 2
        val res = go(from, mid, idx * 2 + 1).min(go(mid, to, idx * 2 + 2))
        arr(idx) = res
        res
      }
    go(0, lcp.length, 0)
    arr
  }
}


