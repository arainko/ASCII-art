import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import scala.annotation.tailrec

object ASCIIart extends App{

    print("Image filepath: ")
    val img = ImageIO.read(new File(io.StdIn.readLine))
    
    def getBrightnessMatrix(img:BufferedImage): Seq[Seq[Int]] = {
        @tailrec
        def matrixHelper(img:BufferedImage, col:Int, row:Int, tempOut: Seq[Int], output:Seq[Seq[Int]]) : Seq[Seq[Int]] = {
            val color = img.getRGB(col,row)
            val red = (color << 8) >>> 24
            val green = (color << 16) >>> 24
            val blue = (color << 24) >>> 24
            val avg = (red+green+blue)/3

            if (col == img.getWidth-1 && row == img.getHeight-1) output :+ (tempOut :+ avg)
            else if (col == img.getWidth-1) matrixHelper(img, 0, row+1, Seq.empty[Int], output :+ tempOut)
            else matrixHelper(img, col+1, row, tempOut :+ avg, output)
        }
        matrixHelper(img, 0, 0, Seq.empty[Int], Seq.empty[Seq[Int]])
    }

    def matrixConvert(seq:Seq[Seq[Int]]): Seq[Seq[Char]] = {
        @tailrec
        def convertHelper(seq: Seq[Seq[Int]], insideSeq: Seq[Int], index:Int, tempOut:Seq[Char], output: Seq[Seq[Char]]) : Seq[Seq[Char]] = {
            val encodeSeq= "`^\",:;Il!i~+_-?][}{1)(|\\/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$".toSeq
            if (seq.tail.isEmpty) output
            else if (insideSeq.isEmpty) convertHelper(seq.tail, seq.tail.head, 1, Seq.empty[Char], output :+ (tempOut :+ '\n'))
            else if (insideSeq.head >= (index*4)-4 && insideSeq.head < index*4) convertHelper(seq, insideSeq.tail, 1, tempOut :+ encodeSeq(index) :+ encodeSeq(index) :+  encodeSeq(index), output)
            else convertHelper(seq, insideSeq, index+1, tempOut, output)
        }
        convertHelper(seq, seq.head, 1, Seq.empty[Char], Seq.empty[Seq[Char]])
    }

    def printMatrix[A](seq: Seq[Seq[A]]): Unit = {
        @tailrec
        def printHelper(seq: Seq[Seq[A]], tempSeq: Seq[A]):  Unit = {
            seq.tail.isEmpty match {
                case true => {
                    print(seq.head.mkString(""))
                    return ()
                }
                case false => {
                    print(tempSeq.mkString(""))
                    printHelper(seq.tail, seq.tail.head)
                }
            }
        }
        printHelper(seq, seq.head)
    }

    val brightnessMatrix = getBrightnessMatrix(img)
    val brightnessToCharMatrix = matrixConvert(brightnessMatrix)
    printMatrix(brightnessToCharMatrix)
    
}

 