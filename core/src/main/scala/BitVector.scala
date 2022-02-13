package tsetlin 

import java.lang.Integer.bitCount
import scala.util.Random
import BitVector._

import Bits.ChangeMasks

/*
 * Mutable bit vector backed by an integer array. The BitVector can wrap an external int array, bits can only
 * start on a int boundary but the vector can have any number of bits. 
 */
final class BitVector(
    private[tsetlin] val underlying: Array[Int],
    // in bits
    val size: Int) {

  @inline def change = size % 32
  @inline def intCount = Bits.closest32(size) / 32

  @inline def intOf(i: Int): Int = underlying(i)

  def isEmpty: Boolean = {
    var i = intCount - 1

    if ((this.intOf(i) & ChangeMasks(change)) != 0) {
      return false
    }

    i -= 1

    while (i >= 0) {
      if (this.intOf(i) != 0) {
        return false
      }

      i -= 1
    }

    true
  }

  inline def apply(i: Int): Boolean = {
    val int = (i / 32)
    val bit = i % 32

    (underlying(int) & (1 << bit)) != 0
  }

  def set(i: Int): Unit = {
    val int = (i / 32)
    val bit = i % 32

    underlying(int) |= (1 << bit)
  }

  def unset(i: Int): Unit = {
    val int = (i / 32)
    val bit = i % 32

    underlying(int) &= (~(1 << bit))
  }

  def popcount: Int = {
    //if (size == 0) return 0

    val len = intCount - 1

    try {
      var count = 0
    
      var i = 0

      while (i < len) {
        count += bitCount(underlying(i))
        i += 1
      }

      count += bitCount(ChangeMasks(change) & underlying(i))

      count
    } catch {
      case t: RuntimeException =>
        println(underlying.mkString("[", ", ", "]"))
        println("size: " + size)
        throw t
    }
  }

  override def toString = (0 until size).map { b =>
    if (apply(b)) "1" else "0"
  }.mkString("BitVector(", ", ", ")")
}

object BitVector {
  val _1 = true
  val _0 = false

  def apply(b0: Boolean, bs: Boolean*): BitVector = {
    val size = 1 + bs.size
    val underlying = reserveArray(1 + size)

    val v = new BitVector(underlying, size)
    if (b0) v.set(0)
    bs.indices.foreach { i =>
      if (bs(i)) {
        v.set(i + 1)
      }
    }

    v
  }

  def from(seq: Seq[Boolean]): BitVector = {
    BitVector(seq(0), seq.tail : _ *)
  }

  private[tsetlin] def reserveArray(nBits: Int): Array[Int] = {
    val nInts = math.ceil(nBits / 32.0).toInt
    new Array(nInts)
  }

  def wrapUnsafe(array: Array[Int], nBits: Int): BitVector = {
    new BitVector(array, nBits)
  }

  def zeroes(nBits: Int): BitVector = {
    val array = reserveArray(nBits)
    new BitVector(array, nBits)
  }
}
