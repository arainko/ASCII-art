import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage

object ASCIIart extends App{
    val img = ImageIO.read(new File("image.jpg"))
    
    def avgColorTable(img:BufferedImage) : Seq[Seq[Int]] = {
        def tableHelper(img:BufferedImage, col:Int, row:Int, tempOut: Seq[Int], output:Seq[Seq[Int]]) : Seq[Seq[Int]] = {
            val color = img.getRGB(col,row)
                val red = (color << 8) >>> 24
                val green = (color << 16) >>> 24
                val blue = (color << 24) >>> 24
                    val avg = (red+green+blue)/3

            if (col == img.getWidth-1 && row == img.getHeight-1) output :+ (tempOut :+ avg)
            else if (col == img.getWidth-1) tableHelper(img, 0, row+1, Seq.empty[Int], output :+ tempOut)
            else tableHelper(img, col+1, row, tempOut :+ avg, output)
        }
        tableHelper(img, 0, 0, Seq.empty[Int], Seq.empty[Seq[Int]])
    }

    def tableConvert(seq:Seq[Seq[Int]]) : Seq[Seq[Char]] = {
        def convertHelper(seq: Seq[Seq[Int]], insideSeq: Seq[Int], index:Int, tempOut:Seq[Char], output: Seq[Seq[Char]]) : Seq[Seq[Char]] = {
            val encodeSeq= "`^\",:;Il!i~+_-?][}{1)(|\\/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$".toSeq

            if (seq.tail.isEmpty) output
            else if (insideSeq.isEmpty) convertHelper(seq.tail, seq.tail.head, 0, Seq.empty[Char], output :+ (tempOut :+ '\n'))
            else if (insideSeq.head >= (index*4)-4 && insideSeq.head < index*4) convertHelper(seq, insideSeq.tail, 0, tempOut :+ encodeSeq(index) :+ encodeSeq(index) :+  encodeSeq(index), output)
            else convertHelper(seq, insideSeq, index+1, tempOut, output)

        }
        convertHelper(seq, seq.head, 0, Seq.empty[Char], Seq.empty[Seq[Char]])
    }

    val a = tableConvert(avgColorTable(img))
    var i = 0
    var j = 0
    while (i < a.size){
        while(j < a(0).size){
            print(a(i)(j))
            j += 1
        }
        j = 0
        i += 1
    }
    
    }

 