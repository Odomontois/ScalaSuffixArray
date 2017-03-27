package odo.suffixarr

object SAISJMaker extends SuffixArray.Maker[Any] {
  private def mkSuffixArray[A](seq: IndexedSeq[A], base: SAISJBase) = {
    val arr = base.suffixArray
    new SuffixArrayImpl[A](seq, arr, SuffixArray.lcpFromSA(seq, arr))
  }

  def bsize[C](implicit be: BoundEnum[C]) = be.size

  def apply[C1 <: Any](seq: IndexedSeq[C1])(implicit boundEnum: BoundEnum[C1]): SuffixArray[C1] = {
    val intSeq = new IntSeq {
      def size: Int = seq.size
      def get(idx: Int): Int = boundEnum.toInt(seq(idx))
    }

    mkSuffixArray(seq, new SAISJ(intSeq, boundEnum.size))
  }

  object ascii extends SuffixArray.Maker[Char] {
    def apply[C <: Char : BoundEnum](seq: IndexedSeq[C]) = mkSuffixArray[C](seq, new SAISJascii(seq.mkString))
  }

  object array extends SuffixArray.Maker[Int] {
    def apply[C <: Int : BoundEnum](seq: IndexedSeq[C]) = mkSuffixArray[C](seq, new SAISJArray(seq.toArray, bsize[C]))
  }

  object shortArray extends SuffixArray.Maker[Short] {
    def apply[C <: Short : BoundEnum](seq: IndexedSeq[C]) = mkSuffixArray[C](seq, new SAISJShortArray(seq.toArray, bsize[C]))
  }
}
