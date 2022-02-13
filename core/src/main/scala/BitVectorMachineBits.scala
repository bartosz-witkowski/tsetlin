package tsetlin

import scala.util.Random
import BitVectorMachineBits._
import Tsetlin._

import jdk.incubator.vector.IntVector
import jdk.incubator.vector.VectorOperators

import java.util.concurrent.atomic.AtomicLong

final class BitVectorMachineBits(
    private[tsetlin] val positiveClassClauses: Array[Clauses],
    private[tsetlin] val negativeClassClauses: Array[Clauses])(
    implicit val description: Tsetlin.Description)
  extends Tsetlin.MutableMachine {

  val workspaces: Array[Workspace] = Array.fill(positiveClassClauses.size)(freshWorkspace)
  def freshWorkspace = Workspace.fresh(description)

  def predict(x: BitVector): Int = {
    val ws = freshWorkspace

    (0 until description.nClasses).map { cls =>
      classOutput(cls, x, true, ws)
      val sum = ws.clauseOutputPositive.popcount - ws.clauseOutputNegative.popcount

      cls -> sum
    }.maxBy(_._2)._1
  }

  def clauseOutputOf(
      clauses: Clauses,
      x: BitVector,
      predict: Boolean,
      clauseOutput: BitVector): Unit = {
    inline def binarizePredict(classOutput: Int): Boolean = {
      classOutput == 1
    }
    inline def binarizeClassify(classOutput: Int): Boolean = {
      classOutput != -1
    }

    val array = clauseOutput.underlying
    val teams = clauses.teams

    var clauseIndex = 0
    val nClauses = description.nClauses

    var int = 0
    var mask = 1
    var outputIndex = 0
    val stop = (1 << 31)

    while (clauseIndex < nClauses) {
      val output = teams(clauseIndex).clauseOutputVec(x)

      val boolean = if (predict) binarizePredict(output) else binarizeClassify(output)
      if (boolean) {
        int |= mask
      }

      if (mask == stop) {
        array(outputIndex) = int
        outputIndex += 1
        mask = 1
        int = 0
      } else {
        mask = (mask << 1)
      }

      clauseIndex += 1
    }

    if (clauseIndex % 32 != 0) {
      array(outputIndex) = int
    }
  }

  def classOutput(cls: Int, x: BitVector, predict: Boolean, ws: Workspace): Unit = {
    clauseOutputOf(positiveClassClauses(cls), x, predict, ws.clauseOutputPositive)
    clauseOutputOf(negativeClassClauses(cls), x, predict, ws.clauseOutputNegative)
  }

  def generateClauseFeedback(
      feedbackProbability: Double,
      feedbackingValue: Byte,
      random: Random,
      feedback: Array[Byte]): Unit = {

    var i = 0
    val size = feedback.size
    while (i < size) {
      feedback(i) = if (random.nextDouble <= feedbackProbability) {
        feedbackingValue
      } else {
        0
      }
      i += 1
    }
  }

  def generateClassFeedback(
      isTargetClass: Boolean,
      clippedSum: Clipped,
      random: Random,
      ws: Workspace): Unit = {

    val threshold = description.threshold

    val feedbackProbability = if (isTargetClass) {
      (threshold - clippedSum.value) / (2.0 * threshold)
    } else {
      (threshold + clippedSum.value) / (2.0 * threshold)
    }

    def f(v: Byte, fs: Array[Byte]): Unit = generateClauseFeedback(feedbackProbability, v, random, fs)

    if (isTargetClass) {
      f(1, ws.positiveFeedback) 
      f(2, ws.negativeFeedback)
    } else {
      f(2, ws.positiveFeedback) 
      f(1, ws.negativeFeedback)
    }
  }

  def applyType1FeedbackOnClauseOutputBoostPositiveFeedbackVec(
      team: Team,
      x: BitVector,
      random: Random): Unit = {
    var i = 0

    val size = team.nInts
    val bound = IntVectorSpecies.loopBound(size)

    val normal = team.normal
    val negated = team.negated

    val xs = x.underlying
    val len = IntVectorSpecies.length

    while (i < bound) {
      val whichBits = IntVector.fromArray(IntVectorSpecies, xs, i)

      // if (x(k)) actionIncludeReinforceInclude --{
      normal.incBitsVec(i, whichBits)
      // }---
        
      // if (!x(k)) actionIncludeNegatedReinforceInclude --{
      negated.incBitsVec(i, whichBits.not)
      // }---
      
      // if x(k) actionIncludeNegatedReinforceExclude --{
      negated.decBitsVec(i, whichBits.and(stream1overS.sampleVec(random)))
      // }---

      // if (!x(k)) actionIncludeReinforceExclude --{
      normal.decBitsVec(i, stream1overS.sampleVec(random).and(whichBits.not))
      // }---
      
      i += len
    }

    while (i < size) {
      val whichBits = xs(i)

      // if (x(k)) actionIncludeReinforceInclude --{
      normal.incBitsInt(i, whichBits)
      // }---
        
      // if (!x(k)) actionIncludeNegatedReinforceInclude --{
      negated.incBitsInt(i, ~whichBits)
      // }---
      
      // if x(k) actionIncludeNegatedReinforceExclude --{
      negated.decBitsInt(i, stream1overS.sample(random) &  whichBits)
      // }---

      // if (!x(k)) actionIncludeReinforceExclude --{
      normal.decBitsInt(i, stream1overS.sample(random) & ~whichBits)
      // }---
      
      i += 1
    }
  }

  def applyType1FeedbackOnClauseOutput(
      team: Team,
      x: BitVector,
      random: Random): Unit = {
    var i = 0

    val normal = team.normal
    val negated = team.negated
    val xs = x.underlying

    val size = team.nInts
    while (i < size) {
      val whichBits = xs(i) 

      // if (x(k)) actionIncludeReinforceInclude --{
      normal.incBitsInt(i, streamDecSOverS.sample(random) & whichBits)
      // }---
        
      // if (!x(k)) actionIncludeNegatedReinforceInclude --{
      negated.incBitsInt(i, streamDecSOverS.sample(random) & ~whichBits)
      // }---
      
      // if x(k) actionIncludeNegatedReinforceExclude --{
      negated.decBitsInt(i, stream1overS.sample(random) & whichBits)
      // }---

      // if (!x(k)) actionIncludeReinforceExclude --{
      normal.decBitsInt(i, stream1overS.sample(random) & ~whichBits)
      // }---
      
      
      i += 1
    }
  }

  class Stream(p: Double) {
    private[this] val stream = {
      val random = new scala.util.Random(3)
      val array = new Array[Int](30 * 1000)
      var i = 0
      while (i < array.size) {
        array(i) = uniformRandomInt(random, p)
        i += 1
      }
      array
    }

    inline def sampleVec(random: Random): IntVector = {
      val offset = random.nextInt(stream.size - IntVectorSpecies.length)
      IntVector.fromArray(IntVectorSpecies, stream, offset)
    }

    inline def sample(random: Random): Int = {
      stream(random.nextInt(stream.size))
    }
  }

  val stream1overS = new Stream(1.0 / description.s)
  val streamDecSOverS = new Stream((description.s - 1.0) / description.s)

  def applyType1FeedbackNoClauseOutputVec(
      team: Team,
      random: Random): Unit = {
    var i = 0

    val nInts = team.nInts
    val normal = team.normal
    val negated = team.negated

    val bound = IntVectorSpecies.loopBound(nInts)
    val len = IntVectorSpecies.length

    while (i < bound) {
      normal.decBitsVec(i, stream1overS.sampleVec(random))
      negated.decBitsVec(i, stream1overS.sampleVec(random))
      i += len
    }

    while (i < nInts) {
      normal.decBitsInt(i, stream1overS.sample(random))
      negated.decBitsInt(i, stream1overS.sample(random))
      i += 1
    }
  }

  def applyType2FeedbackVec(
      team: Team,
      x: BitVector): Unit = {
    val nInts = team.nInts
    var i = 0

    val xs = x.underlying
    val bound = IntVectorSpecies.loopBound(nInts)

    val normal = team.normal
    val negated = team.negated
    val len = IntVectorSpecies.length

    while (i < bound) {
      val xvec = IntVector.fromArray(IntVectorSpecies, xs, i)

      {
        val whichBits = xvec.and(negated.includeVecOf(i).not())
        negated.incBitsVec(i, whichBits)
      }

      {
        val whichBits = (xvec.or(normal.includeVecOf(i))).not()
        normal.incBitsVec(i, whichBits)
      }

      i += len
    }

    while (i < nInts) {
      val xInt = xs(i)

      {
        val whichBits = xInt & (~negated.includeBitsOf(i))
        negated.incBitsInt(i, whichBits)
      }

      {
        val whichBits = ~(xInt | normal.includeBitsOf(i))
        normal.incBitsInt(i, whichBits)
      }

      i += 1
    }
  }

  def applyFeedbackToClauses(
      clauseVotes: BitVector,
      clauses: Clauses,
      feedbacks: Array[Byte],
      x: BitVector,
      random: Random): Unit = {

    var i = 0
    val size = description.nClauses
    val teams = clauses.teams

    while (i < size) {
      val team = teams(i)
      feedbacks(i) match {
        case 1 =>
          if (clauseVotes(i)) {
            //if (description.boostTruePositiveFeedback) {
              applyType1FeedbackOnClauseOutputBoostPositiveFeedbackVec(team, x, random)
            //} else {
            //applyType1FeedbackOnClauseOutput(team, x, random)
          //}
          } else {
            applyType1FeedbackNoClauseOutputVec(team, random)
          }

        case 2 =>
          if (clauseVotes(i)) {
            applyType2FeedbackVec(teams(i), x)
          }

        case _ =>
          ()
      }

      i += 1
    }
  }

  def applyFeedback(
      cls: Int,
      x: BitVector,
      random: Random,
      ws: Workspace): Unit = {

    applyFeedbackToClauses(ws.clauseOutputPositive, positiveClassClauses(cls), ws.positiveFeedback, x, random)
    applyFeedbackToClauses(ws.clauseOutputNegative, negativeClassClauses(cls), ws.negativeFeedback, x, random)
  }

  def traceDump(): String = (
    s"pos: sync($tSync1) ws($tWs1) class-output($tClassOutput1) gen-feedback($tGenerateClassFeedback1) apply($tApplyFeedback1)\n" +
    s"neg: sync($tSync2) ws($tWs2) class-output($tClassOutput2) gen-feedback($tGenerateClassFeedback2) apply($tApplyFeedback2) rand($tPaired)\n"
  )

  var tSync1 = 0L
  var tWs1 = 0L
  var tClassOutput1 = 0L
  var tGenerateClassFeedback1 = 0L
  var tApplyFeedback1 = 0L

  var tPaired = 0L

  var tSync2 = 0L
  var tWs2 = 0L
  var tClassOutput2 = 0L
  var tGenerateClassFeedback2 = 0L
  var tApplyFeedback2 = 0L

    
  def update(x: BitVector, y: Int, random: Random): Unit = {
    val targetClass = y

    var t6 = 0L

    val t0 = System.currentTimeMillis
    positiveClassClauses(targetClass).synchronized {
      val t1 = System.currentTimeMillis
      val ws = workspaces(targetClass)
      val t2 = System.currentTimeMillis
      classOutput(targetClass, x, predict = false, ws)
      val t3 = System.currentTimeMillis

      generateClassFeedback(
        isTargetClass = true, 
        ClassOutput.sumUpClauseVotes(ws.clauseOutputPositive, ws.clauseOutputNegative),
        random,
        ws)
      val t5 = System.currentTimeMillis

      applyFeedback(
        targetClass, 
        x,
        random,
        ws)

      t6 = System.currentTimeMillis
      tSync1 += t1 - t0
      tWs1 += t2 - t1
      tClassOutput1 += t3 - t2
      tGenerateClassFeedback1 += t5 - t3
      tApplyFeedback1 += t6 - t5
    }

    val pairedClass: Int = {
      val nClasses = description.nClasses
      /*
       * Note: index != y and 0 <= index < nClasses
       *
       * Proof 
       *   r = random.nextInt(nClasses - 1) ∈ 1, 2 ... nClasses - 2
       *
       *   1 + r + y (mod nClasses) = y  => ∃ k ∈ N |
       *     1 + r + y = (k * nClasses) + y =>
       *     1 + r = k * nClasses
       *
       *     if k = 1
       *       1 + r = nClasses but r < nClasses - 2 => r + 1 != nClasses
       *     otherwise 
       *     1 + r << (k * nClasses)
       */
      (1 + random.nextInt(nClasses - 1) + y) % nClasses
    }
    val t7 = System.currentTimeMillis

    positiveClassClauses(pairedClass).synchronized {
      val t8 = System.currentTimeMillis
      val ws = workspaces(pairedClass)
      val t9 = System.currentTimeMillis
      classOutput(pairedClass, x, predict = false, ws)
      val t10 = System.currentTimeMillis

      generateClassFeedback(
        isTargetClass = false, 
        ClassOutput.sumUpClauseVotes(ws.clauseOutputPositive, ws.clauseOutputNegative), 
        random,
        ws)
      val t11 = System.currentTimeMillis

      applyFeedback(
        pairedClass, 
        x,
        random,
        ws)
      val t12 = System.currentTimeMillis

      tPaired = t7 - t6
      tSync2 += t8 - t7
      tWs2 += t9 - t8
      tClassOutput2 += t10 - t9
      tGenerateClassFeedback2 += t11 - t10
      tApplyFeedback2 += t12 - t11
    }
  }

  def stateString = (
    "BitVectorMachineBits(\n" + 
    "\t"+positiveClassClauses.map(_.stateString).mkString("Array(", ", ", ")") + "\n" +
    "\t"+negativeClassClauses.map(_.stateString).mkString("Array(", ", ", ")") +
    ")"
  )

  override def toString = stateString
}

object BitVectorMachineBits {
  def uniformRandomInt(random: Random, p: Double ): Int = {
    var i = 32
    var value = 0

    while (i >= 0) {
      if (random.nextDouble() <= p) {
        value = value | 1
      }
      if (i != 0) {
        value = value << 1
      }

      i -= 1
    }

    value;
  }

  def flipInt(random: Random, p: Double, value: Int): Int = {
    var out = value;
    
    var i = 31
    var pattern = (1 << 31)
    
    while (i >= 0) {
      if ((out & pattern) != 0 && random.nextDouble() <= p) {
        out &= ~pattern
      }
      pattern = (pattern >>> 1)
      i -= 1
    }
    
    out
  }

  final class Clauses(
    private[tsetlin] val teams: Array[Team])(implicit description: Description) {
  
    def teamAtIndex(index: Int): Team = teams(index)

    def stateString = teams.indices.map { i =>
      teamAtIndex(i).stateString
    }.mkString("Clauses(", ", ", ")")

    override def toString = stateString
  }

  val IntVectorSpecies = IntVector.SPECIES_PREFERRED

    /*
     * underlying(0): Ints such that underlying(i / 32) = has the (i % 32) bit set if clause(i) is in the include state 
     * underlying(1): 
     *      .
     *      .
     *      .
     * underlying(N): 
     *
     * N = ceil(log_2(nStates + 1))
     */
  final class State(
      private[tsetlin] val nBits: Int,
      private[tsetlin] val nInts: Int,
      private val underlying: Array[Int]) {

    inline def includeBitsOf(i: Int): Int = underlying(i)
    inline def includeVecOf(i: Int): IntVector = IntVector.fromArray(IntVectorSpecies, underlying, i)

    def includeBit(k: Int): Boolean = {
      (includeBitsOf(k / 32) & (1 << (k % 32))) != 0
    }

    inline def get(bit: Int, int: Int) = underlying((bit * nInts) + int)

    def count(bit: Int): Int = {
      var i = nBits - 1
      
      var acc = 0
      var pow = 1

      val intIndex = bit / 32
      @inline def bitInInt = bit % 32
      val mask = (1 << bitInInt)

      while (i >= 0) {
        val int = get(i, intIndex)

        if ((int & mask) != 0) {
          acc |= pow
        }

        pow = pow << 1

        i -= 1
      }

      acc
    }

    inline def offsetForIndex(index: Int): Int = ((nBits - 1) * nInts) + index

    def incBitsVec(
        index: Int,
        firstCarry: IntVector): Unit = {
      val under = underlying
      var nextCarry = firstCarry

      var offset = offsetForIndex(index)

      while (offset >= 0) {
        if (nextCarry.compare(VectorOperators.EQ, 0).allTrue) {
          return 
        }
        val carry = nextCarry

        val bits = IntVector.fromArray(IntVectorSpecies, under, offset)
        nextCarry = bits.and(carry)

        bits.lanewise(VectorOperators.XOR, carry).intoArray(under, offset)

        offset -= nInts
      }

      if (nextCarry.compare(VectorOperators.EQ, 0).allTrue) {
        return
      }
      
      offset = offsetForIndex(index)

      while (offset >= 0) {
        IntVector.fromArray(IntVectorSpecies, under, offset).or(nextCarry).intoArray(under, offset)
        offset -= nInts
      }
    }

    def decBitsVec(
        index: Int,
        firstCarry: IntVector): Unit = {
      val under = underlying
      var nextCarry = firstCarry
      var offset = offsetForIndex(index)

      while (offset >= 0) {
        if (nextCarry.compare(VectorOperators.EQ, 0).allTrue) {
          return 
        }
        val carry = nextCarry

        val bits = IntVector.fromArray(IntVectorSpecies, under, offset)
        nextCarry = (bits.not).and(carry)

        bits.lanewise(VectorOperators.XOR, carry).intoArray(under, offset)

        offset -= nInts
      }

      if (nextCarry.compare(VectorOperators.EQ, 0).allTrue) {
        return
      }
      
      nextCarry = nextCarry.not

      offset = offsetForIndex(index)

      while (offset >= 0) {
        IntVector.fromArray(IntVectorSpecies, under, offset).and(nextCarry).intoArray(under, offset)
        offset -= nInts
      }
    }

    def incBitsInt(
        index: Int,
        firstCarry: Int): Unit = {
      var offset = offsetForIndex(index)
      var nextCarry = firstCarry

      val under = underlying

      while (offset >= 0) {
        if (nextCarry == 0) {
          return 
        }
        val carry = nextCarry

        nextCarry = under(offset) & carry
        under(offset) ^= carry

        offset -= nInts
      }

      if (nextCarry == 0) return
      
      offset = offsetForIndex(index)

      while (offset >= 0) {
        under(offset) |= nextCarry
        offset -= nInts
      }
    }

    def decBitsInt(
        index: Int,
        firstCarry: Int): Unit = {
      var offset = offsetForIndex(index)
      var nextCarry = firstCarry 
      val under = underlying

      while (offset >= 0) {
        if (nextCarry == 0) {
          return
        }

        val carry = nextCarry
        nextCarry = ~under(offset) & carry
        under(offset) ^= carry

        offset -= nInts
      }

      if (nextCarry == 0) return

      offset = offsetForIndex(index)

      while (offset >= 0) {
        under(offset) &= ~nextCarry
        offset -= nInts
      }
    }
  }

  object State {
    def initial(nBits: Int, nDimensions: Int): State = {
      var i = 0
      
      val nInts = Bits.closest32(nDimensions) / 32
      val underlying = new Array[Int](nBits * nInts)

      java.util.Arrays.fill(underlying, nInts, underlying.size, -1)

      new State(nBits, nInts, underlying)
    }
  }

  final class Team(
    private[tsetlin] val normal: State,
    private[tsetlin] val negated: State)(implicit description: Description) {

    @inline def nInts = normal.nInts

    override def toString = patternString

    def patternString = {
      val include = (0 until description.nDimensions).map { i =>
        if (actionInclude(i)) "+" else " "
      }.mkString("[", "", "]")

      val includeNegated = (0 until description.nDimensions).map { i =>
        if (actionIncludeNegated(i)) "-" else " "
      }.mkString("[", "", "]")

      s"Team($include, $includeNegated)"
    }

    def stateString = {
      (0 until description.nDimensions).map { i =>
        normal.count(i)
      }.mkString("[", ", ", "]")  ++
      (0 until description.nDimensions).map { i =>
        negated.count(i)
      }.mkString("[", ", ", "]")
    }

    def actionInclude(k: Int): Boolean = {
      normal.includeBit(k)
    }

    def actionIncludeNegated(k: Int): Boolean = {
      negated.includeBit(k)
    }

    inline def clauseOutputVec(vec: BitVector): Byte = {
      var literals = false
      var output = true

      var i = 0
      val xs = vec.underlying
      val size = xs.length - 2

      val bound = IntVectorSpecies.loopBound(size)

      while (output && i < bound) {
        val x = IntVector.fromArray(IntVectorSpecies, xs, i)
        val include = normal.includeVecOf(i)
        val negatedInclude = negated.includeVecOf(i)

        literals = literals || (include.compare(VectorOperators.NE, 0).anyTrue) || (negatedInclude.compare(VectorOperators.NE, 0).anyTrue)
           
        if (
          (include.and(x.not).compare(VectorOperators.NE, 0).anyTrue) || 
          (negatedInclude.and(x).compare(VectorOperators.NE, 0).anyTrue)
        ) {
          output = false
        }

        i += IntVectorSpecies.length
      }

      while (output && i < size) {
        val x = xs(i)
        val include = normal.includeBitsOf(i)
        val negatedInclude = negated.includeBitsOf(i)

        literals = literals || (include != 0) || (negatedInclude != 0)
           
        if (((include & ~x) != 0) || ((negatedInclude & x) != 0)) {
          output = false
        }

        i += 1
      }

      if (!output) {
        -1
      } else {
        val mask = Bits.ChangeMasks(description.nDimensions % 32)

        val x = xs(i) & mask
        val include = normal.includeBitsOf(i) & mask
        val negatedInclude = negated.includeBitsOf(i) & mask

        literals = literals || (include != 0) || (negatedInclude != 0)

        if (((include & ~x) != 0) || ((negatedInclude & x) != 0)) {
          -1
        } else if (!literals) {
          0
        } else {
          1
        }
      }
    }
  }

  def fromSimpleMachine(
      machine: SimpleMutableMachine): BitVectorMachineBits = {
    import machine.description
    val bm = initial(description)

    def copy(
        classClauses: Array[SimpleMutableMachine.Clauses],
        classClausesTarget: Array[Clauses]): Unit = {
      classClauses.zipWithIndex.foreach { case (clauses, cls) =>
        clauses.foreachTeamIndex { i =>
          val targetTeam = classClausesTarget(cls).teamAtIndex(i)

          (0 until description.nDimensions * 2).foreach { k =>
            val value = clauses.teamAtIndex(i).automatonAt(k).value

            val state = if (k % 2 == 0) targetTeam.normal else targetTeam.negated

            val n = k / 2

            val index = n / 32
            val bit = n % 32
            val mask = 1 << bit

            if (value >= description.nStates) {
              val count = (value - description.nStates) + 1
              (0 until count).foreach { _ =>
                state.incBitsInt(index, mask)
              }
            } else {
              val count = (description.nStates - value) - 1

              (0 until count).foreach { _ =>
                state.decBitsInt(index, mask)
              }
            }
          }
        }
      }
    }
    copy(machine.positiveClassClauses, bm.positiveClassClauses)
    copy(machine.negativeClassClauses, bm.negativeClassClauses)

    bm
  }

  def initial(description: Description): BitVectorMachineBits = {
    implicit val d = description

    def log2(x: Double): Double = math.log10(x) / math.log10(2.0)
    // we need to represent 0.. 2 * nStates 
    val N = math.ceil(log2(d.nStates + 1)).toInt

    def occupyClauses: Array[Clauses] = {
      val clauses = new Array[Clauses](d.nClasses)

      clauses.indices.foreach { cls =>
        def state = State.initial(N, d.nDimensions)

        val teams = new Array[Team](d.nClauses)

        teams.indices.foreach { n =>
          teams(n) = new Team(
            normal  = state,
            negated = state)
        }

        clauses(cls) = new Clauses(teams)
      }

      clauses
    }

    val positiveClassClauses = occupyClauses
    val negativeClassClauses = occupyClauses

    new BitVectorMachineBits(positiveClassClauses, negativeClassClauses)
  }

  case class Workspace(
    clauseOutputPositive: BitVector,
    clauseOutputNegative: BitVector,
    positiveFeedback: Array[Byte],
    negativeFeedback: Array[Byte])

  object Workspace {
    def fresh(implicit d: Description): Workspace = {
      Workspace(
        BitVector.zeroes(d.nClauses),
        BitVector.zeroes(d.nClauses),
        new Array[Byte](d.nClauses),
        new Array[Byte](d.nClauses))
    }
  }
}
