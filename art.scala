import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import scala.annotation.tailrec

object ASCIIart extends App{

    print("Image filepath: ")
    val path = io.StdIn.readLine
    val img = ImageIO.read(new File(path))
    
    def getBrightnessMatrix(img:BufferedImage): Seq[Seq[Int]] = {
        @tailrec
        def matrixHelper(img:BufferedImage, col:Int, row:Int, tempOut: Seq[Int], output:Seq[Seq[Int]]) : Seq[Seq[Int]] = {
            val color = img.getRGB(col,row)
            val red = (color << 8) >>> 24
            val green = (color << 16) >>> 24
            val blue = (color << 24) >>> 24
            val avg = (red+green+blue)/3

            col match {
                case col if (col == img.getWidth-1) => row match {
                    case row if (row == img.getHeight-1) => output :+ (tempOut :+ avg)
                    case _ => matrixHelper(img, 0, row+1, Seq.empty[Int], output :+ (tempOut :+ avg))
                }
                case _ => matrixHelper(img, col+1, row, tempOut :+ avg, output)
            }
        }
        matrixHelper(img, 0, 0, Seq.empty[Int], Seq.empty[Seq[Int]])
    }

    def matrixConvert(seq:Seq[Seq[Int]]): Seq[Seq[Char]] = {
        @tailrec
        def convertHelper(seq: Seq[Seq[Int]], insideSeq: Seq[Int], index:Int, tempOut:Seq[Char], output: Seq[Seq[Char]]) : Seq[Seq[Char]] = {
            val encodeStr= "`^\",:;Il!i~+_-?][}{1)(|\\/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$"
           
            seq match {
                case Seq() => output
                // single element handle
                case Seq(upperHead) => insideSeq match {
                    case Seq() => convertHelper(Seq(), Seq(), 0, tempOut, output :+ (tempOut :+ '\n'))
                    case (head +: tail) => head match {
                        case head if (head >= (index*4)-4 && head < index*4) => 
                            convertHelper(seq, tail, 0, tempOut :+ encodeStr(index) :+ encodeStr(index) :+ encodeStr(index), output)
                        case _ => convertHelper(seq, insideSeq, index+1, tempOut, output)
                    }
                }
                // multiple element handle
                case (upperHead +: upperTail) => insideSeq match {
                    case Seq() => convertHelper(upperTail, upperTail.head, 0, Seq.empty[Char], output :+ (tempOut :+ '\n'))
                    case (head +: tail) => head match {
                        case head if (head >= (index*4)-4 && head < index*4) => 
                            convertHelper(seq, tail, 0, tempOut :+ encodeStr(index) :+ encodeStr(index) :+ encodeStr(index), output)
                        case _ => convertHelper(seq, insideSeq, index+1, tempOut, output)
                    }
                }

            }
        }
        convertHelper(seq, seq.head, 0, Seq.empty[Char], Seq.empty[Seq[Char]])
    }

    def printMatrix[A](seq: Seq[Seq[A]]): Unit = {
        @tailrec
        def printHelper(seq: Seq[Seq[A]], tempSeq: Seq[A]):  Unit = seq.tail.isEmpty match {
            case true => {
                print(seq.head.mkString(""))
                return ()
            }
            case false => {
                print(tempSeq.mkString(""))
                printHelper(seq.tail, seq.tail.head)
            }
        }

        printHelper(seq, seq.head)
    }

    val brightnessMatrix = getBrightnessMatrix(img)
    val brightnessToCharMatrix = matrixConvert(brightnessMatrix)
    printMatrix(brightnessToCharMatrix)
    
}

 