/**
  * Created by irik on 26.04.16.
  */
package GIFReader
import scodec._
import bits._
import scodec.codecs._
import java.nio.file.{Files, Paths}
import java.io._

import scodec.bits.ByteOrdering.LittleEndian

import swing.{Panel, MainFrame, SimpleSwingApplication}
import java.awt.{Color, Graphics2D, Dimension}
import java.awt.image.BufferedImage


class DataPanel(data: Array[Array[Color]]) extends Panel {

  override def paintComponent(g: Graphics2D) {
    val width = data.length
    val height = data.map(_.length).max
    val dx = g.getClipBounds.width.toFloat  / width
    val dy = g.getClipBounds.height.toFloat / height
    for {
      x <- 0 until data.length
      y <- 0 until data(x).length
      x1 = (x * dx).toInt
      y1 = (y * dy).toInt
      x2 = ((x + 1) * dx).toInt
      y2 = ((y + 1) * dy).toInt
    } {
      data(x)(y) match {
        case c: Color => g.setColor(c)
        case _ => g.setColor(Color.WHITE)
      }

      g.fillRect(x1, y1, x2 - x1, y2 - y1)
    }
    /* Alternative
    val i = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    for {
      x <- 0 until data.length
      y <- 0 until data(x).length
    } {
      data(x)(y) match {
        case c: Color => i.setRGB(x, y, data(x)(y).getRGB)
        case _ => i.setRGB(x, y, Color.WHITE.getRGB)
      }

    }

    g.drawRenderedImage(i, java.awt.geom.AffineTransform.getScaleInstance(dx, dy))
    */
  }

}

object render extends SimpleSwingApplication {

  val width = 25;
  val height = 25;
  val scale = 8;
  val data = Array.ofDim[Color](width, height)

  // plot some points
  data(0)(0) = Color.BLACK
  data(4)(4) = Color.RED
  data(0)(4) = Color.GREEN
  data(4)(0) = Color.BLUE


  def top = new MainFrame {
    contents = new DataPanel(data) {
      preferredSize = new Dimension(width * scale, height * scale)
    }
  }
}



case class GifHead(sizes: GIFSizes, BG: Int, palitrpar: PalitraParametrs, GlobalPalitra: Array[BitVector])
case class GifBody(GraphContrList: List[GraphicControl], TextList: List[PlainText], ImPar: List[ImageParamet], ImList: List[Images])

case class GIFSizes(width : Int, height : Int)

case class PalitraParametrs(deep: Int, size: Int)
case class ImageParamet(left: Int, top: Int, pH: Int, pW: Int)

case class Images(v: BitVector,palitraParam: PalitraParametrs, palitra: Array[BitVector])

case class GraphicControl(disp: Int, other: Int, delay: Int, Tr: Int)
case class TextPar(left: Int, top: Int, W:Int, H: Int, cW: Int, cH: Int, TextCol: Int, BG: Int)
case class PlainText(par: TextPar, Text: Array[Char])


class Decoder(path : String) {

  def head (bitVector: BitVector): GIFSizes = {
    try {
      val headerCodec = (constant(hex"474946383961".bits) :: uint16L :: uint16L).as[GIFSizes] //GIF89a
      val decoded = headerCodec.decode(bitVector)
      println(decoded.require.value)
      return decoded.require.value
    }
    catch {
      case e: IllegalArgumentException => {
        val headerCodec = (constant(hex"474946383761".bits) :: uint16L :: uint16L).as[GIFSizes] //GIF87a
        val decoded = headerCodec.decode(bitVector)
        println(decoded.require.value)
        return decoded.require.value
      }
    }
  }

  def pal(pa: BitVector): PalitraParametrs = {
 println(pa)
    val d = pa.drop(1).take(3).toInt(false,LittleEndian)
    val s = pa.drop(4).take(4).toInt(false,LittleEndian)
    val par = PalitraParametrs(d + 1, (math.pow(2, s + 1).toInt))
    return par
  }

  def colors (c: BitVector, p: PalitraParametrs): Array[BitVector] ={
    val ColAr = new Array[BitVector](p.size) // Если цвет не совпадет, поменять на бигэндиан
    var col= c
    for{i<- 0 until p.size }{
      ColAr(i) = col.take(3 * 8)
      println(ColAr(i).toHex)
      col = col.drop(3 * 8)
    }
    return ColAr
  }


  def headsplit(bitvec: BitVector): GifHead = {
    val Sizes = head(bitvec)
    var bitv = bitvec.drop(48 + 32) // 48 - GifHead + 32 - w h
    if (bitv(0) == false) {
      bitv = bitv.drop(8)
      val BG = uint8L
      val decoded = BG.decode(bitv)
      println(decoded.require.value)
      return  GifHead(Sizes, decoded.require.value, PalitraParametrs(0,0), null)
    }
    else {
      val Glparam = pal(bitv.take(8))
      println(Glparam)
      bitv = bitv.drop(8)

      val BG = uint8L
      val decoded = BG.decode(bitv)
      println(decoded.require.value)
      bitv = bitv.drop(16)

      return  GifHead(Sizes,decoded.require.value,Glparam,(colors(bitv, Glparam)))

    }
  }


  def split(vec: BitVector): BitVector = {
    def cicle (vect: BitVector, lzwim: BitVector): BitVector = {
      println(vect)
      val s = vect.take(8).toInt(false, LittleEndian)
      println(s)
      if (s == 0) lzwim
      else  cicle(vect.drop(8 + s * 8),lzwim.++(vect.drop(8).take(s * 8)))
    }
    cicle(vec.drop(8), vec.take(0))
  }

  def dr (v: BitVector): Int = {
    def cicle (vec: BitVector, d: Int): Int = {
      val s = vec.take(8).toInt(false, LittleEndian)
      if (s == 0) d+8
      else cicle(vec.drop(8 + s * 8), d + 8 + s * 8)
    }
    cicle(v, 0)
  }


  def bodydecode (gifH: GifHead, v: BitVector, GraphContrList: List[GraphicControl], TextList: List[PlainText], ImPar: List[ImageParamet], ImList: List[Images]): GifBody = {

    if (hex"3b".bits == v.take(8)) GifBody(GraphContrList, TextList, ImPar, ImList)
    else if (hex"2c".bits == v.take(8)) {
      println("image" + v.take(100))
      var vec = v.drop(8)
      val par = (uint16L :: uint16L :: uint16L :: uint16L).as[ImageParamet]
      val decoded = par.decode(vec)
      println(decoded.require.value)
      vec = vec.drop(16 * 4)
      if (vec.take(1) == true) {
        val loc = pal(vec.take(8))
        println(loc)
        val pict = Images(split(vec.drop(8 + 8 * 3 * loc.size)),loc,colors(vec.drop(8), loc))
        println(pict)
        if (pict.v.length % 255 == 0) bodydecode(gifH, vec.drop(16+8 + 8 * 3 * loc.size + pict.v.length + 8 * (pict.v.length / 255)), GraphContrList, TextList, decoded.require.value :: ImPar, pict :: ImList)
        else bodydecode(gifH,vec.drop(16+8 + 8 * 3 * loc.size + pict.v.length + 8 * (pict.v.length / 255) + 8), GraphContrList, TextList, decoded.require.value :: ImPar, pict :: ImList)

      }
      else{
        val pict = Images(split(vec.drop(8)),gifH.palitrpar,gifH.GlobalPalitra)
        println(pict)
        if (pict.v.length % 255 == 0) bodydecode(gifH, vec.drop(16+8 + pict.v.length + 8 * (pict.v.length / 255)), GraphContrList, TextList, decoded.require.value :: ImPar, pict :: ImList)
        else bodydecode(gifH,vec.drop(16+8 + pict.v.length + 8 * (pict.v.length / 255) + 8), GraphContrList, TextList, decoded.require.value :: ImPar, pict :: ImList)
      }
    }
    else{

      if (hex"f9".bits == v.drop(8).take(8) ){  //Graphic Control Extension Block
        println("graphic" + v.take(100))
        val vec = v.drop(16 + 8 + 3)
        val grap = (uintL(3):: uintL(2):: uint16L :: uint8L).as[GraphicControl]
        val decoded = grap.decode(vec)
        println(decoded.require.value)
        bodydecode(gifH, vec.drop(3 + 2 + 16 + 8 + 8),decoded.require.value::GraphContrList,TextList,ImPar,ImList)
      }
      else if (hex"fe".bits == v.drop(8).take(8)) {  //Comment Extension Block
        println("comment" + v.take(100))
        val d = dr(v.drop(8+8))
        bodydecode(gifH, v.drop(d + 16 + 8),GraphContrList,TextList,ImPar,ImList)
      }
      else if(hex"01".bits == v.drop(8).take(8)){  //Plain Text Extension Block
        println("text" + v.take(100))
        val vec = v.drop(16 + 8)
        val param = (uint16L::uint16L::uint16L::uint16L::uint8L::uint8L::uint8L::uint8L).as[TextPar]
        val decoded = param.decode(vec)
        println(decoded.require.value)
        val text = split(vec.drop(12 * 8))
        val bytetext = text.toByteArray
        val t = new Array[Char](bytetext.length)
        for {i<- 0 until bytetext.length}{
          t(i) = bytetext(i).toChar
        }
        val txt = PlainText(decoded.require.value,t)
        if (text.length % 255 == 0) bodydecode(gifH, vec.drop(12 * 8 + text.length + 8 * (text.length / 255)),GraphContrList,txt::TextList,ImPar,ImList)
        else  bodydecode(gifH, vec.drop(12 * 8 + text.length + 8 * (text.length / 255) + 8),GraphContrList,txt::TextList,ImPar,ImList)
      }
      else{ //Application Extension Block
        println("application" + v.take(100))
        bodydecode(gifH, v.drop(19 * 8),GraphContrList,TextList,ImPar,ImList)
      }

    }

  }


  val byteArray = Files.readAllBytes(Paths.get(path))
  var bitVector = BitVector(byteArray)
println(bitVector.take(100))

  val HeadOfGIF = headsplit(bitVector)
  println(HeadOfGIF)

  bitVector = bitVector.drop(13 * 8 + HeadOfGIF.palitrpar.size * 8 * 3)
  println(bitVector.take(100))

  val body = bodydecode(HeadOfGIF,bitVector, Nil, Nil, Nil, Nil)
  println(body)
}


object test {
  def main(args: Array[String]) {
    new Decoder("animgif.gif");

  }
}
