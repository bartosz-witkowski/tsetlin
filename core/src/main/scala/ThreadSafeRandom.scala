package tsetlin

import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import java.util.concurrent.ThreadLocalRandom 

/**
 * Proxies ThreadLocalRandom.current as Random
 */
class ThreadSafeRandom extends Random {
  inline private[this] def r = ThreadLocalRandom.current

  override def nextBoolean(): Boolean = r.nextBoolean
  override def nextBytes(bytes: Array[Byte]): Unit = r.nextBytes(bytes) 
  override def nextDouble(): Double = r.nextDouble
  override def nextFloat(): Float = r.nextFloat()
  override def nextGaussian(): Double = r.nextGaussian()
  override def nextInt(): Int = r.nextInt()
  override def nextInt(n: Int): Int = r.nextInt(n)
  override def nextLong(): Long = r.nextLong()
  override def nextString(length: Int) = {
    val safe = "qwertyuiopasdfghjklzxcvbnm1234567890QWERTYUIOPASDFGHJKLZXCVBNM!@#$%^&*()_+{}:"

    (0 until length).map { _ =>
      safe(nextInt(safe.size))
    }.mkString
  }
  override def nextPrintableChar(): Char = {
    val low = 33
    val high = 127

    (r.nextInt(high - low) + low).toChar
  }

  override def setSeed(seed: Long): Unit = { 
    () 
  }

  /*
   * https://github.com/scala/scala/blob/v2.13.6/src/library/scala/util/Random.scala#L22
   */
  override def shuffle[T, C](xs: IterableOnce[T])(implicit bf: scala.collection.BuildFrom[xs.type, T, C]): C = {
    val buf = new ArrayBuffer[T] ++= xs

    def swap(i1: Int, i2: Int): Unit = {
      val tmp = buf(i1)
      buf(i1) = buf(i2)
      buf(i2) = tmp
    }

    for (n <- buf.length to 2 by -1) {
      val k = nextInt(n)
      swap(n - 1, k)
    }

    (bf.newBuilder(xs) ++= buf).result()
  }

  override def alphanumeric: LazyList[Char] = {
    def nextAlphaNum: Char = {
      val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
      chars charAt (r.nextInt(chars.length))
    }

    LazyList continually nextAlphaNum
  }
}
