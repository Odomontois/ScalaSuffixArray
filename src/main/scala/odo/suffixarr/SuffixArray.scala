package odo.suffixarr

import spire.syntax.cfor._

import scala.collection.mutable

abstract class SuffixArray[+C] {
  def original: IndexedSeq[C]
  def suffixArray: IndexedSeq[Int]
  def LCP: IndexedSeq[Int]
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
      val suffixArray: IndexedSeq[Int] = seq.tails.map(_.toIterable).toArray.sorted.map(seq.size - _.size)
      val LCP: IndexedSeq[Int] = suffixArray.iterator.zip(suffixArray.tail.iterator).map { case (x, y) ⇒ lcp(seq.drop(x), seq.drop(y)) }.toArray
    }
  }
}

object SA_ISMaker extends SuffixArray.Maker[Any] {
  def apply[T: BoundEnum](seq: IndexedSeq[T]): SuffixArray[T] = new SA_ISMaker(seq).build
}

class SA_ISMaker[C](val seq: IndexedSeq[C])(implicit bounds: BoundEnum[C]) {
  val n = seq.size

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

  def showTypeMap = (0 to n).map(i ⇒ if (typeMap(i)) "L" else if (isLMS(i)) "[S]" else "S").mkString

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

  def build: SuffixArray[C] = ???

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

    def printAt(i: Int): Unit = {
      println(arr.zipWithIndex.map{
        case (e,`i`) => "[" + e + "]"
        case (e, _)  => e
      }.mkString(", "))
    }

    def induceSortL: this.type = {
      val bh = bucketHeads

      cforRange(0 until n) { i ⇒
        if (arr(i) >= 0) {
          val j = arr(i) - 1
          if (j >= 0 && typeMap(j)) {
            val bIndex = bounds.toInt(seq(j))
            arr(bh(bIndex)) = j
            bh(bIndex) += 1

            printAt(i)
          }
        }
      }
      this
    }

    def induceSortS: this.type = {
      val bt = bucketTails

      cforRange(n to 0 by -1){ i =>
        val j = arr(i) - 1
        if(j >= 0 && !typeMap(j)){
          val bIndex = bounds.toInt(seq(j))
          arr(bt(bIndex)) = j
          bt(bIndex) -= 1

          printAt(i)
        }
      }
      this
    }

    def summarize = {
      val lmsNames = Array.fill(n + 1)(-1)
      var currentName = 0
      lmsNames(arr(0)) = currentName
      var last: Int = arr(0)
      var count = 1

      cforRange(1 to n){ i =>
        val current = arr(i)
        if(isLMS(current)){
          if(!lmsSubsEqual(current, last)) currentName += 1
          last = current
          lmsNames(current) = currentName
          count += 1

          println(arr.toSeq)
        }
      }


      val size = currentName + 1
      val offsets = Array.ofDim[Int](count)
      val names = Array.ofDim[Int](count)

      lmsNames.iterator.zipWithIndex.filter(_._1 != -1).zipWithIndex.foreach{
        case ((offset, name), idx) =>
          offsets(idx) = offset
          names(idx) = name
      }

      Summary(names, size, offsets)
    }

    override def toString = arr.mkString(s"Produced($seq)[", ", ", "]")
  }

  case class Summary(names: Array[Int], size: Int, offsets: Array[Int]){
    override def toString = s"Summary(names = ${names.mkString("[", ", ", "]")}; size = $size; offsets= ${offsets.mkString("[", ", ", "]")})"
  }
}
