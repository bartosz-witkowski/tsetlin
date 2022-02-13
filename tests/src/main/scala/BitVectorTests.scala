package tsetlin

import tsetlin.BitVector
import BitVector._

object BitVectorTests {
  def test1(): Unit = {
    val b = BitVector(_1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1, _1)

    assert(b.size == 66)
    assert(b.popcount == b.size)
    assert(b.intCount == 3)

    (0 until b.size).foreach { i =>
      b.unset(i)

      assert(b.popcount == (b.size -1))

      b.set(i)
    }
  }

  def test2(): Unit = {
    val b = BitVector.zeroes(49)
    b.set(48)
    assert(b.popcount == 1)
  }

  def test3(): Unit = {
    (0 until 4000).foreach { size =>
      val b = BitVector.zeroes(size)
      (0 until size).foreach { i =>
        b.set(i)

        assert (b.popcount == 1)

        b.unset(i)
      }
    }
  }

  def test4(): Unit = {
    val array = BitVector.reserveArray(125)
    val b = BitVector.wrapUnsafe(array, 125)

    (0 until b.size).foreach { i =>
      b.set(i)
      assert(b.popcount == i + 1)
    }
  }

  def run(): Unit = {
    test1()
    test2()
    test3()
    test4()
  }
}
