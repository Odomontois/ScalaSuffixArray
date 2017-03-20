package odo.suffixarr

trait BoundEnum[T] extends Ordering[T] {
  self ⇒
  def lower: T
  def higher: T
  def toInt(x: T): Int
  def fromInt(x: Int): T
  def next(x: T): T = fromInt(toInt(x) + 1)
  def prev(x: T): T = fromInt(toInt(x) - 1)
  def size: Int = toInt(higher) - toInt(lower) + 1

  def slice(from: T, to: T) = new BoundEnum[T] {
    def lower: T = from
    def higher: T = to
    def toInt(x: T): Int = self.toInt(x) - self.toInt(from)
    def fromInt(x: Int): T = self.fromInt(x + self.toInt(from))
    def compare(x: T, y: T): Int = self.compare(x, y)
  }
}

object BoundEnum {
  implicit val char = new BoundEnum[Char] {
    def lower: Char = '\u0000'
    def higher: Char = '\u00ff'
    def compare(x: Char, y: Char): Int = x - y
    def toInt(x: Char): Int = x.toInt
    def fromInt(x: Int): Char = x.toChar
    override def size = 256
  }

  implicit val byte = new BoundEnum[Byte] {
    def lower: Byte = Byte.MinValue
    def higher: Byte = Byte.MaxValue
    def compare(x: Byte, y: Byte): Int = x - y
    def toInt(x: Byte): Int = x.toInt
    def fromInt(x: Int): Byte = x.toByte
    override def size = 256
  }

  implicit def tuple[A, B](implicit A: BoundEnum[A], B: BoundEnum[B]) = new BoundEnum[(A, B)] {
    def lower: (A, B) = (A.lower, B.lower)
    def higher: (A, B) = (A.higher, B.higher)
    def toInt(x: (A, B)): Int = A.toInt(x._1) * B.size + B.toInt(x._2)
    def fromInt(x: Int): (A, B) = (A.fromInt(x / B.size), B.fromInt(x % B.size))
    def compare(x: (A, B), y: (A, B)): Int = {
      val cmp1 = A.compare(x._1, y._1)
      if (cmp1 == 0) B.compare(x._2, y._2) else cmp1
    }
  }

  def forElems[T](elems: TraversableOnce[T])(implicit ord: Ordering[T]): BoundEnum[T] = new BoundEnum[T] {
    val elemSeq = elems.toSet.toIndexedSeq.sorted
    val elemMap = elemSeq.zipWithIndex.toMap
    def lower: T = elemSeq.head
    def higher: T = elemSeq.last
    def toInt(x: T): Int = elemMap(x)
    def fromInt(x: Int): T = elemSeq(x)
    def compare(x: T, y: T): Int = ord.compare(x, y)
  }
}
