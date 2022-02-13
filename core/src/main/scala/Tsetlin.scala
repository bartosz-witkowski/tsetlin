package tsetlin

import scala.util.Random

import scala.collection.parallel.CollectionConverters._
import java.io.DataInputStream

object Tsetlin {
  class Clipped private[tsetlin] (val value: Int) extends AnyVal {
    override def toString = s"Clipped($value)"
  }

  private[tsetlin] def clip(i: Int)(implicit description: Description): Clipped = {
    new Clipped(
      if (i > description.threshold) {
        description.threshold
      } else if (i < -description.threshold) {
        description.threshold
      } else {
        i
      }
    )
  }

  case class Description(
      // the input will have nDimensions bits (o)
      nDimensions: Int,
      // how many classes to train against
      nClasses: Int,
      // each class will learn learn via "nClauses" x positive-clauses and "nClauses" x negative-clauses
      nClauses: Int,
      // training parameter: precision
      s: Double,
      // each tsetlin machine has nStates negative states and nStates positive states
      nStates: Int,
      // training parameter: T
      threshold: Int,
      // training parameter: should true positives and negatives be always feedbacked?
      boostTruePositiveFeedback: Boolean) {

    def ===(that: Description): Boolean = {
      this == that
    }
  }

  sealed abstract class ClauseOutput
  object ClauseOutput {
    case object NoLiterals extends ClauseOutput
    case object Match extends ClauseOutput
    case object Conflict extends ClauseOutput
  }

  case class ClassOutput(
      positive: BitVector,
      negative: BitVector) {

    override def toString = s"ClassOutput(${positive.popcount}, ${negative.popcount})"

    def sumUpClauseVotes(implicit description: Description): Clipped = {
      ClassOutput.sumUpClauseVotes(positive, negative)
    }
  }
  object ClassOutput {
    def sumUpClauseVotes(
        positive: BitVector,
        negative: BitVector)(implicit description: Description): Clipped = {
      clip(positive.popcount - negative.popcount)
    }
  }

  sealed abstract class Feedback
  object Feedback {
    case object Inaction extends Feedback
    case object TypeI extends Feedback
    case object TypeII extends Feedback
  }

  case class ClassFeedback(
      positive: Array[Feedback],
      negative: Array[Feedback])

  abstract class MutableMachine {
    def predict(x: BitVector): Int
    def update(x: BitVector, y: Int, random: Random): Unit
  }

  def evaluate(
      machine: MutableMachine, 
      xs: Array[BitVector],
      ys: Array[Int]): Double = {

    val count = (0 until xs.size).count { i =>
      machine.predict(xs(i)) == ys(i)
    }

    count.toDouble / xs.size
  }

  def parallelEvaluate(
      machine: MutableMachine, 
      xs: Array[BitVector],
      ys: Array[Int], 
      groupSize: Int = 32): Double = {

    val count = (0 until xs.size).par.count { i =>
      machine.predict(xs(i)) == ys(i)
    }

    count.toDouble / xs.size
  }

  def fit(
      machine: MutableMachine,
      xs: Array[BitVector],
      ys: Array[Int],
      examplesPerEpoch: Int,
      epochs: Int,
      random: Random): Unit = {

    val indices = (0 until xs.size).toArray

    var epoch = 0
    while (epoch < epochs) {
      val t0 = System.currentTimeMillis
      shuffle(random, indices)
      val t1 = System.currentTimeMillis

      val t2 = System.currentTimeMillis
      var index = 0
      var counter = 0
      while (index < examplesPerEpoch) {
        counter += 1
        if (counter % 100 == 0) {
          val ms = System.currentTimeMillis - t2
          
          println(s"${counter / (ms.toDouble / 1000.0)} ops/s")
        }
        val i = indices(index)
        val x = xs(i)
        val y = ys(i)
        machine.update(x, y, random)
        index += 1
      }

      epoch += 1
    }
  }

  def shuffle(random: Random, array: Array[Int]): Unit = {
    def swap(i: Int, j: Int): Unit = {
      array(i) ^= array(j)
      array(j) ^= array(i)
      array(i) ^= array(j)
    }

    var i = array.size - 1
    while (i > 0) {
      val j = random.nextInt(i)
      swap(i, j)
      
      i -= 1
    }
  }

  def parallelFit(
      machine: MutableMachine,
      xs: Array[BitVector],
      ys: Array[Int],
      examplesPerEpoch: Int,
      epochs: Int,
      random: Random): Unit = {

    val indices = (0 until xs.size).toArray

    var epoch = 0

    while (epoch < epochs) {
      val t0 = System.currentTimeMillis
      shuffle(random, indices)
      val t1 = System.currentTimeMillis

      val t2 = System.currentTimeMillis

      val counter = new java.util.concurrent.atomic.AtomicInteger(0)
      val t = System.currentTimeMillis

      (0 until examplesPerEpoch).par.foreach { index =>
        if (counter.incrementAndGet % 100 == 0) {
          val ms = System.currentTimeMillis - t
          
          println(s"${counter.get / (ms.toDouble / 1000.0)} ops/s")
        }
        val i = indices(index)
        val x = xs(i)
        val y = ys(i)

        try {
          machine.update(x, y, random)
        } catch {
          case t: Throwable =>
            t.printStackTrace
        }
      }

      epoch += 1
    }
  }
}
