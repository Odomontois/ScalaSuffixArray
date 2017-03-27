package odo.suffixarr

object SAISJasciiMaker extends SuffixArray.Maker[Char] {
  def apply[C1 <: Any](seq: IndexedSeq[C1])(implicit boundEnum: BoundEnum[C1]): SuffixArray[C1] = {

    val arr = new SAISJascii(seq.mkString).suffixArray

    new SuffixArrayImpl[C1](seq, arr, SuffixArray.lcpFromSA(seq, arr))
  }
}
