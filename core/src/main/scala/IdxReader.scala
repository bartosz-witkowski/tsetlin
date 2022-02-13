package tsetlin

import java.io.ByteArrayOutputStream;
import java.nio.ByteBuffer;
import java.io.{FileInputStream, InputStream}

/*
 * A simplified idx [http://yann.lecun.com/exdb/mnist/] file reader.
 */
object IdxReader {
  def readImages(fileName: String): Array[Array[Int]] = {
    val is = new FileInputStream(fileName) 
    val header = readImageHeader(is)

    val array = new Array[Array[Int]](header.numberOfItems)

    val imageSize = header.numberOfCols * header.numberOfRows
    val count = header.numberOfItems

    array.indices.foreach { i =>
      array(i) = nextVector(is, imageSize)
    }
    array
  }

  def readLabels(fileName: String): Array[Int] = {
    val is = new FileInputStream(fileName) 
    val header = readLabelHeader(is)

    val count = header.numberOfItems
    val array = new Array[Int](header.numberOfItems)

    array.indices.foreach { i =>
      array(i) = nextVector(is, 1).apply(0)
    }

    array
  }

  def readLabelN(fileName: String, index: Int): Int = {
    val is = new FileInputStream(fileName) 

    val header = readLabelHeader(is)
    val itemCount = header.numberOfItems

    is.skip(index)
  
    val item = nextVector(is, 1).apply(0)
    is.close
    item
  }

  def readEntryN(fileName: String, index: Int): Array[Int] = {
    val is = new FileInputStream(fileName) 

    val header = readImageHeader(is)
    val itemSize = header.numberOfRows * header.numberOfCols

    if (index > 0) { 
      is.skip(itemSize * index)
    }

    val item = nextVector(is, itemSize)
    is.close
    item
  }

  private case class LabelHeader(numberOfItems: Int)

  private case class ImageHeader(
    numberOfItems: Int,
    numberOfRows: Int,
    numberOfCols: Int)

  private def readImageHeader(is: InputStream): ImageHeader = {
    // omit magic number
    nextInt(is)

    val itemCount = nextInt(is)
    val rowCount = nextInt(is)
    val colCount = nextInt(is)

    ImageHeader(itemCount, rowCount, colCount)
  }

  private def toInt(b: Byte): Int = (b & 0xFF)

  private def readLabelHeader(is: InputStream): LabelHeader = {
    // omit magic number
    nextInt(is)

    val itemCount = nextInt(is)

    LabelHeader(itemCount)
  }

  private def nextVector(is: InputStream, nBytes: Int): Array[Int] = {
    val buffer = new Array[Byte](nBytes);
    is.read(buffer, 0, buffer.length)
    buffer.map(toInt)
  }

  private def nextInt(is: InputStream): Int = {
    val buffer = new Array[Byte](4);
    is.read(buffer, 0, buffer.length)
    ByteBuffer.wrap(buffer, 0, 4).getInt
  }

  val Range = " ░▒▓█"

  def mkImageString(array: Array[Int], width: Int): String = {
    array.map { i => 
      val bucket = 255 / Range.size
      val r = math.min(i / bucket, Range.size - 1)
      Range(r)
    }.grouped(width).map(_.mkString).mkString("\n")
  }
}
