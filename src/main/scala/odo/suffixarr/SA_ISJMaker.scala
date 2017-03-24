package odo.suffixarr

object SA_ISJMaker extends SuffixArray.Maker[Any] {
  def apply[C1 <: Any](seq: IndexedSeq[C1])(implicit boundEnum: BoundEnum[C1]): SuffixArray[C1] = {
    val intSeq = new IntSeq {
      def size: Int = seq.size
      def get(idx: Int): Int = boundEnum.toInt(seq(idx))
    }

    val arr = new SA_ISJ(intSeq, boundEnum.size).suffixArray

    new SuffixArrayImpl[C1](seq, arr, SuffixArray.lcpFromSA(seq, arr))
  }
}
