package tsetlin

import java.io.File

object Control {
  val parallel = true
  val nIterations = 50
}

object Main {
  def binarizeMnist(raw: Array[Array[Int]]): Array[BitVector] = {
    raw.map { image =>
      BitVector.from(
        image.map { pixel =>
          (pixel / 255.0 > 0.3)
        }
      )
    }
  }

  def mnist(dir: String): Unit = {
    val (trainingData, testData) = {
      val rawTrainingData = IdxReader.readImages(s"$dir/train-images-idx3-ubyte")
      println(IdxReader.mkImageString(rawTrainingData(13), 28))
      val rawTestData = IdxReader.readImages(s"$dir/t10k-images-idx3-ubyte")

      @inline def trainingData = binarizeMnist(rawTrainingData)
      @inline def testData = binarizeMnist(rawTestData)

      (trainingData, testData)
    }

    val trainingLabels = IdxReader.readLabels(s"$dir/train-labels-idx1-ubyte")
    val testLabels = IdxReader.readLabels(s"$dir/t10k-labels-idx1-ubyte")

    /*
    */

    val description = Tsetlin.Description(
      nDimensions = trainingData.head.size,
      nClauses = 1000,
      nClasses = 10,
      s = 10.0,
      nStates = 512,
      threshold = 50,
      boostTruePositiveFeedback = true)

    val random = new ThreadSafeRandom 

    //val machine = SimpleMutableMachine.from(description, random)
    val machine = BitVectorMachineBits.initial(description)

    import java.io._

    (0 until Control.nIterations).foreach { iter =>
      val t0 = System.currentTimeMillis

      val ranMethodName = if (Control.parallel) {
        // run
        Tsetlin.parallelFit(
          machine,
          trainingData,
          trainingLabels,
          examplesPerEpoch = 30000,
          epochs = 1,
          random)

        "parallelFit"
      } else {
        // run
        Tsetlin.fit(
          machine,
          trainingData,
          trainingLabels,
          examplesPerEpoch = 30000,
          epochs = 1,
          random)
        "fit"
      }
      val t1 = System.currentTimeMillis

      println(s"Tsetlin.${ranMethodName} took ${t1 - t0}")
      val t2 = System.currentTimeMillis
      println(s"iteration $iter effectiveness ${Tsetlin.parallelEvaluate(machine, testData.take(300), testLabels.take(300))}")
      val t3 = System.currentTimeMillis
      println(s"Tsetlin.parallelEvaluate took ${t3 - t2}")

      // TODO: snapshots
      println(" DONE!")
    }

    println(s"Trained machine effectiveness ${Tsetlin.parallelEvaluate(machine, testData, testLabels)}")
  }

  def main(args: Array[String]): Unit = {
    //mnist("fashionMnist")
    mnist("mnist")
  }
}
