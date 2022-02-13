package tsetlin

import Tsetlin._
import scala.util.Random
import SimpleMutableMachine._

/*
 * * A single automaton belongs to a team.
 * * Each team learns learns a single clause
 *   * the team consists of (2 * "nDimensions") automata 
 *   * a positive clause recognizes 1-bits that it "wants" and 0-bits that it wants
 *   * a negative clause recognizes 1-bits that it "doesn't want" and 0-bits that it doesn't want
 */
class SimpleMutableMachine(
    private[tsetlin] val positiveClassClauses: Array[Clauses],
    private[tsetlin] val negativeClassClauses: Array[Clauses])(
    implicit val description: Description) 
  extends Tsetlin.MutableMachine {

  import description._

  def predict(x: BitVector): Int = {
    val predictions = (0 until nClasses).map { cls =>
      val output = classOutput(cls, x, true)

      cls -> output
    }

    predictions.map { case (cls, output) =>
      val sum = output.positive.popcount - output.negative.popcount

      cls -> sum
    }.maxBy(_._2)._1
  }

  def clauseOutputOf(
      clauses: Clauses,
      x: BitVector,
      predict: Boolean): BitVector = {
    def binarizePredict(classOutput: ClauseOutput): Boolean = {
      classOutput == ClauseOutput.Match
    }
    def binarizeClassify(classOutput: ClauseOutput): Boolean = {
      classOutput != ClauseOutput.Conflict
    }

    val clauseOutput = BitVector.zeroes(nClauses)
    
    (0 until nClauses).foreach { clauseIndex =>
      val output = clauses.teamAtIndex(clauseIndex).clauseOutput(x)
      val boolean = if (predict) binarizePredict(output) else binarizeClassify(output)

      if (boolean) {
        clauseOutput.set(clauseIndex)
      }
    }

    clauseOutput
  }

  def classOutput(cls: Int, x: BitVector, predict: Boolean): ClassOutput = {
    val positive = clauseOutputOf(positiveClassClauses(cls), x, predict)
    val negative = clauseOutputOf(negativeClassClauses(cls), x, predict)

    ClassOutput(positive, negative)
  }

  def generateClauseFeedback(
      feedbackProbability: Double,
      feedbackingValue: Feedback,
      random: Random): Array[Feedback] = {

    val feedback = new Array[Feedback](nClauses)

    feedback.indices.foreach { i =>
      feedback(i) = if (random.nextDouble() <= feedbackProbability) {
        feedbackingValue
      } else {
        Feedback.Inaction
      }
    }

    feedback
  }

  def generateClassFeedback(
      isTargetClass: Boolean,
      clippedSum: Clipped,
      random: Random): ClassFeedback = {

    val feedbackProbability = if (isTargetClass) {
      (threshold - clippedSum.value) / (2.0 * threshold)
    } else {
      (threshold + clippedSum.value) / (2.0 * threshold)
    }

    def f(v: Feedback): Array[Feedback] = generateClauseFeedback(feedbackProbability, v, random)

    if (isTargetClass) {
      ClassFeedback(f(Feedback.TypeI), f(Feedback.TypeII))
    } else {
      ClassFeedback(f(Feedback.TypeII), f(Feedback.TypeI))
    }
  }

  object applyType1FeedbackNoClauseOutput {
    val p = 1.0 / description.s

    def apply(
        team: Team,
        random: Random): Unit = {
      (0 until nDimensions).foreach { k =>
        if (random.nextDouble() < p) team.actionIncludeReinforceExclude(k)
        if (random.nextDouble() < p) team.actionIncludeNegatedReinforceExclude(k)
      }
    }
  }

  object applyType1FeedbackOnClauseOutput {
    val p = (description.s - 1.0) / description.s
    
    def apply(
        team: Team,
        x: BitVector,
        random: Random): Unit = {
      (0 until nDimensions).foreach { k =>
        if (x(k)) {
          if (description.boostTruePositiveFeedback || random.nextDouble() < p) team.actionIncludeReinforceInclude(k)
          if (random.nextDouble() < p)                                          team.actionIncludeNegatedReinforceExclude(k)
        } else {
          if (description.boostTruePositiveFeedback || random.nextDouble() < p) team.actionIncludeNegatedReinforceInclude(k)
          if (random.nextDouble() < p)                                          team.actionIncludeReinforceExclude(k)
        }
      }
    }
  }

  def applyType2Feedback(
      team: Team,
      x: BitVector): Unit = {
    (0 until nDimensions).foreach { k =>
      val actionInclude = team.actionInclude(k)
      val actionIncludeNegated = team.actionIncludeNegated(k)

      if (x(k) && !actionIncludeNegated) {
        team.actionIncludeNegatedReinforceInclude(k)
      } else if (!x(k) && !actionInclude) {
        team.actionIncludeReinforceInclude(k)
      }
    }
  }

  def applyFeedbackToClauses(
      clauseVotes: BitVector,
      clauses: Clauses,
      feedbacks: Array[Feedback],
      x: BitVector,
      random: Random): Unit = {

    clauses.foreachTeamIndex { i =>
      val team = clauses.teamAtIndex(i)
      feedbacks(i) match {
        case Feedback.TypeI =>
          if (clauseVotes(i)) {
            applyType1FeedbackOnClauseOutput(team, x, random)
          } else {
            applyType1FeedbackNoClauseOutput(team, random)
          }

        case Feedback.TypeII =>
          if (clauseVotes(i)) {
            applyType2Feedback(team, x)
          }

        case Feedback.Inaction =>
          ()
      }
    }
  }

  def applyFeedback(
      cls: Int,
      classOutput: ClassOutput,
      feedback: ClassFeedback,
      x: BitVector,
      random: Random): Unit = {

    applyFeedbackToClauses(classOutput.positive, positiveClassClauses(cls), feedback.positive, x, random)
    applyFeedbackToClauses(classOutput.negative, negativeClassClauses(cls), feedback.negative, x, random)
  }

  def update(x: BitVector, y: Int, random: Random): Unit = {
    val pairedClass: Int = {
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
    val targetClass = y

    val classOutputTarget = positiveClassClauses(targetClass).synchronized {
      classOutput(targetClass, x, predict = false)
    }

    val classOutputPaired = positiveClassClauses(pairedClass).synchronized {
      classOutput(pairedClass, x, predict = false)
    }

    positiveClassClauses(targetClass).synchronized {
      val generatedFeedback = generateClassFeedback(isTargetClass = true, classOutputTarget.sumUpClauseVotes, random)

      applyFeedback(
        targetClass, 
        classOutputTarget, 
        generatedFeedback, 
        x,
        random)
    }

    positiveClassClauses(pairedClass).synchronized {
      val generatedFeedback = generateClassFeedback(isTargetClass = false, classOutputPaired.sumUpClauseVotes, random)

      applyFeedback(
        pairedClass, 
        classOutputPaired, 
        generatedFeedback,
        x,
        random)
    }
  }

  def ===(that: SimpleMutableMachine): Boolean = {
    def clausesArrayEquals(xs: Array[Clauses], ys: Array[Clauses]): Boolean = {
      xs.size == ys.size &&
      xs.zip(ys).forall { case (x, y) =>
        x === y
      }
    }

    this.description === that.description &&
    clausesArrayEquals(this.positiveClassClauses, that.positiveClassClauses) &&
    clausesArrayEquals(this.negativeClassClauses, that.negativeClassClauses)
  }

  override def toString = stateString
  def stateString = (
    "SimpleMutableMachine(" + 
      positiveClassClauses.map(_.stateString).mkString("Array(", ", ", ")") + ", " +
      negativeClassClauses.map(_.stateString).mkString("Array(", ", ", ")") + ")"
  )
}

object SimpleMutableMachine {
  class Clauses(teams: Array[Team]) {
    def teamAtIndex(index: Int): Team = teams(index)
    def foreachTeamIndex(f: Int => Unit): Unit = {
      teams.indices.foreach(f)
    }

    def size = teams.size

    def ===(that: Clauses): Boolean = {
      if (this.size == that.size) {
        teams.indices.forall { i =>
          this.teamAtIndex(i) === that.teamAtIndex(i)
        }
      } else {
        false
      }
    }

    def stateString = teams.map(_.stateString).mkString("Clauses(", ", ", ")")
    def patternString = teams.map(_.patternString).mkString("Clauses(", ", ", ")")
  }

  class Team private[SimpleMutableMachine] (
    private[tsetlin] val rawData: Array[Int])(implicit val description: Description) {

    override def toString = patternString

    def ===(that: Team): Boolean = {
      this.rawData.sameElements(that.rawData)
    }
    
    def stateString = {
      val include = (0 until description.nDimensions).map { i =>
        rawData(i * 2)
      }.mkString("[", ", ", "]")

      val includeNegated = (0 until description.nDimensions).map { i =>
        rawData((i * 2) + 1)
      }.mkString("[", ", ", "]")

      s"Team($include, $includeNegated)"
    }

    def patternString = {
      val include = (0 until description.nDimensions).map { i =>
        if (actionInclude(i)) "+" else " "
      }.mkString("[", "", "]")

      val includeNegated = (0 until description.nDimensions).map { i =>
        if (actionIncludeNegated(i)) "-" else " "
      }.mkString("[", "", "]")

      s"Team($include, $includeNegated)"
    }

    private[tsetlin] def reinforceExclude(index: Int): Unit = {
      rawData(index) = Automaton(rawData(index)).nextStateExclude.value
    }

    private[tsetlin] def reinforceInclude(index: Int): Unit = {
      rawData(index) = Automaton(rawData(index)).nextStateInclude.value
    }

    def actionIncludeReinforceInclude(i: Int): Unit = {
      reinforceInclude(i * 2)
    }
    def actionIncludeNegatedReinforceInclude(i: Int): Unit = {
      reinforceInclude((i * 2) + 1)
    }

    def actionIncludeReinforceExclude(i: Int): Unit = {
      reinforceExclude(i * 2)
    }
    def actionIncludeNegatedReinforceExclude(i: Int): Unit = {
      reinforceExclude((i * 2) + 1)
    }

    def automatonAt(index: Int): Automaton = Automaton(rawData(index))

    def actionInclude(i: Int): Boolean = automatonAt(i * 2).action
    def actionIncludeNegated(i: Int): Boolean = automatonAt((i * 2) + 1).action

    def clauseOutput(x: BitVector): ClauseOutput = {
      var i = 0
      var offset = 0

      var output = true
      var noLiterals = true

      while (i < x.size) {
        val bit = x(i)

        val include        = Automaton(rawData(offset)).action
        val negatedInclude = Automaton(rawData(offset + 1)).action

        if (include || negatedInclude) {
          if (!output) return ClauseOutput.Conflict

          noLiterals = false
        }

        if (include && !bit || negatedInclude && bit) {
          if (!noLiterals) return ClauseOutput.Conflict
          output = false
        }

        offset += 2
        i += 1
      }

      if (noLiterals) {
        ClauseOutput.NoLiterals
      } else if (output) {
        ClauseOutput.Match
      } else {
        // shouldn't happen
        ClauseOutput.Conflict
      }
    }
  }

  def from(description: Description, random: Random): SimpleMutableMachine = {
    implicit val d = description
    def randomClauses: Array[Team] = {
      import description._

      val teams = new Array[Team](nClauses)

      teams.indices.foreach { i =>
        val array = new Array[Int](nDimensions * 2)
        array.indices.foreach { j =>
          array(j) = if (random.nextBoolean) { nStates - 1 } else { nStates }
        }

        val team = new Team(array)
        teams(i) = team
      }

      teams
    }

    def classClauess = {
      val array = new Array[Clauses](description.nClasses)
      array.indices.foreach { i =>
        array(i) = new Clauses(randomClauses)
      }
      array
    }

    new SimpleMutableMachine(classClauess, classClauess)
  }

  class Automaton(val value: Int) extends AnyVal {
    def action(implicit desc: Description): Boolean = {
             
      //     negative      positive
      // [ ]  [ ] ... [ ] | [ ] [ ] ... 
      //  \....   ....../   \....   ...'
      //       'v'               'v'
      //      nStates           nStates
      //
      value >= desc.nStates
    }

    def nextStateExclude: Automaton = {
      if (value - 1 >= 0) new Automaton(value - 1) else this
    }

    def nextStateInclude(implicit desc: Description): Automaton = {
      if (value + 1 <= desc.nStates * 2) {
        Automaton(value + 1)
      } else {
        this
      }
    }
  }
  object Automaton {
    def apply(i: Int): Automaton = new Automaton(i)
  }
}
