package render

import java.awt.Color
import java.awt.image.BufferedImage

object Render {
  def render(img: BufferedImage): Unit = { //        img.setRGB(500, 300, new Color(255, 0, 200).getRGB());
  /*  var total: Int = 20;
    for (a <- 1 to total) {
      var angle: Float = a.toFloat / total.toFloat * 360f;
      renderLine(img, 600, 400, 600 + (Math.cos(Math.toRadians(angle)) * 200).toInt, 400 + (Math.sin(Math.toRadians(angle)) * 200).toInt, Color.BLACK)
    }*/
    renderTriangle(img,(100,100),(200,200),(150,100),Color.GREEN)
  }


  //Стоит начать с этого
  def renderLine(img: BufferedImage, x1: Int, y1: Int, x2: Int, y2: Int, color: Color): Unit = {
    if (x1 == x2) {
      for (i <- Math.min(y1, y2) to Math.max(y1, y2)) {
        img.setRGB(x1, i, color.getRGB);
      }
    } else {
      if (Math.abs(y1 - y2) > Math.abs(x1 - x2)) {
        var k: Float = (x1.toFloat - x2.toFloat) / (y1.toFloat - y2.toFloat)
        var b: Float = x1 - k * y1
        for (i <- Math.min(y1, y2) to Math.max(y1, y2)) {
          var y: Float = k * i + b;
          img.setRGB(y.toInt, i, color.getRGB);
        }
      } else {
        var k: Float = (y1.toFloat - y2.toFloat) / (x1.toFloat - x2.toFloat)
        var b: Float = y1 - k * x1
        for (i <- Math.min(x1, x2) to Math.max(x1, x2)) {
          var y: Float = k * i + b;
          img.setRGB(i, y.toInt, color.getRGB);
        }
      }
    }
  }

  def renderTriangle(img: BufferedImage, A: (Int, Int), B: (Int, Int), C: (Int, Int), color: Color): Unit = {
    //      Seq(1,2,3).min
    var minX: Int = A._1.min(B._1).min(C._1)
    var maxX: Int = A._1.max(B._1).max(C._1)
    var minY: Int = A._1.min(B._2).min(C._2)
    var maxY: Int = A._2.max(B._2).max(C._2)

    for (i <- minX to maxX; j <- minY to maxY) {
      if (Rasterization.isPointInTriangle(A, B, C, (i, j))) {
        img.setRGB(i, j, color.getRGB);
      }
    }
  }


}
