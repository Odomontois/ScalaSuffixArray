package odo.suffixarr

object SA_ISJasciiMaker extends SuffixArray.Maker[Char] {
  def apply[C1 <: Any](seq: IndexedSeq[C1])(implicit boundEnum: BoundEnum[C1]): SuffixArray[C1] = {

    val arr = new SA_ISJascii(seq.mkString).suffixArray

    new SuffixArrayImpl[C1](seq, arr, SuffixArray.lcpFromSA(seq, arr))
  }
}
