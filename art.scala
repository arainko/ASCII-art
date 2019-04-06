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

    def matrixConvert(seq:Seq[Seq[Int]]): Seq[Seq[String]] = {
        val encodeStr= "`^\",:;Il!i~+_-?][}{1)(|\\/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$"
        val singleCharMatrix = seq.map(n => n.map (k => encodeStr((k/4).ceil.toInt)))
        singleCharMatrix.map(n => n.map (k => (k.toString)*3))    
    }

    def printMatrix[A](seq: Seq[Seq[A]]): Unit = seq match {
        case Seq() => return ()
        case Seq(single) => {
            print("\n" + single.mkString("") + "\n\n")
            return ()
        }
        case (head +: tail) => {
            print("\n" + head.mkString(""))
            printMatrix(tail)
        }
    }

    val brightnessMatrix = getBrightnessMatrix(img)
    val brightnessToCharMatrix = matrixConvert(brightnessMatrix)
    printMatrix(brightnessToCharMatrix)
    
}

 