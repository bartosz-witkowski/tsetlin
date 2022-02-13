package tsetlin

object Bits {
  private[this] val LowBitMasks: Array[Int] = {
    val array = new Array[Int](32)
    var mask = 0
    var bit = 0

    array.indices.foreach { i =>
      mask |= (1 << bit)
      bit += 1
      array(i) = mask
    }

    array.reverse
  }

  private[tsetlin] val ChangeMasks: Array[Int] = {
    val array = new Array[Int](32)
    array.indices.foreach { change =>
      array(change) = LowBitMasks((32 - change) % 32)
    }

    array
  }

  /*
   * XXX: we want to store classes at the next Int boundary so that concurrent updates do not mess up other
   *      BitVectors and for easier BitVector / BitVector operations
   */
  def closest32(i: Int): Int = if (i % 32 == 0) i else (i | 31) + 1
}
