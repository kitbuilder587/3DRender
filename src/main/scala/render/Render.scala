package render

import java.awt.Color
import java.awt.image.BufferedImage
import render.Matrix
import render.Rasterization
import render.Vector
import render.Main

import java.io.File
import javax.imageio.ImageIO

object Render {

  var cameraPos:Vector = new Vector(0,0,-1)
  var lightPos:Vector = new Vector(0,0,-1)
  var alpha:Double = 0
  var zBuffer:Array[Array[Double]] = null

  def render(img: BufferedImage): Unit = { //        img.setRGB(500, 300, new Color(255, 0, 200).getRGB());
  /*  var total: Int = 20;
    for (a <- 1 to total) {
      var angle: Float = a.toFloat / total.toFloat * 360f;
      renderLine(img, 600, 400, 600 + (Math.cos(Math.toRadians(angle)) * 200).toInt, 400 + (Math.sin(Math.toRadians(angle)) * 200).toInt, Color.BLACK)
    }*/
    //renderTriangle(img,new Vector(100.0,100.0),new Vector(700.0,200.0),new Vector(500.0,500.0),Color.GREEN)
    val texture: BufferedImage = ImageIO.read(new File("uaz.png"));
    zBuffer = new Array[Array[Double]](Main.w)
    renderCube(img,Main.w,Main.h,texture)
    alpha += 0.1
  }


  //Стоит начать с этого
  def renderLine(img: BufferedImage, x1: Int, y1: Int, x2: Int, y2: Int, color: Color): Unit = {
    if (x1 == x2) {
      for (i <- Math.min(y1, y2).max(0) to Math.max(y1, y2).min(Main.h - 1)) {
        img.setRGB(x1.max(0).min(Main.w - 1), i, color.getRGB);
      }
    } else {
      if (Math.abs(y1 - y2) > Math.abs(x1 - x2)) {
        var k: Float = (x1.toFloat - x2.toFloat) / (y1.toFloat - y2.toFloat)
        var b: Float = x1 - k * y1
        for (i <- Math.min(y1, y2).max(0) to Math.max(y1, y2).min(Main.h -1)) {
          var y: Float = k * i + b;
          if(y >= 0 && y < Main.w)
            img.setRGB(y.toInt, i, color.getRGB);
        }
      } else {
        var k: Float = (y1.toFloat - y2.toFloat) / (x1.toFloat - x2.toFloat)
        var b: Float = y1 - k * x1
        for (i <- Math.min(x1, x2).max(0) to Math.max(x1, x2).min(Main.w-1)) {
          var y: Float = k * i + b;
          if(y >= 0 && y < Main.h)
            img.setRGB(i, y.toInt, color.getRGB);
        }
      }
    }
  }

  def renderTriangle(img: BufferedImage, A: Vector, B: Vector, C: Vector, texture:BufferedImage,textureCoardinates: Array[Vector],points: Array[Vector]): Unit = {
    //      Seq(1,2,3).min
    var minX: Int = A.vec(0).min(B.vec(0)).min(C.vec(0)).toInt.max(0).min(Main.w-1)
    var maxX: Int = A.vec(0).max(B.vec(0)).max(C.vec(0)).ceil.toInt.max(0).min(Main.w-1)
    var minY: Int = A.vec(1).min(B.vec(1)).min(C.vec(1)).toInt.max(0).min(Main.h-1)
    var maxY: Int = A.vec(1).max(B.vec(1)).max(C.vec(1)).ceil.toInt.max(0).min(Main.h-1)
    for (i <- minX to maxX; j <- minY to maxY) {
      if (Rasterization.isPointInTriangle((A.vec(0),A.vec(1)), (B.vec(0),B.vec(1)), (C.vec(0),C.vec(1)), (i, j))) {
        var bc:Vector = Rasterization.baracentricCoardinates((A.vec(0),A.vec(1)), (B.vec(0),B.vec(1)), (C.vec(0),C.vec(1)), (i, j))
        var resTextures = textureCoardinates(0).plus(textureCoardinates(1).minus(textureCoardinates(0)).scalarMultiply(bc.vec(0))).plus(textureCoardinates(2).minus(textureCoardinates(0)).scalarMultiply(bc.vec(1)))
        var pointInTriangle =  points(0).plus(points(1).minus(points(0)).scalarMultiply(bc.vec(0)).plus(points(2).minus(points(0).scalarMultiply(bc.vec(1)))))
        try {
         // println(textureCoardinates(1).minus(textureCoardinates(0)).vec(0) +  " " + textureCoardinates(2).minus(textureCoardinates(0)).vec(0))
          if(pointInTriangle.vec(2) < zBuffer(i)(j)) {
            zBuffer(i)(j) = pointInTriangle.vec(2)
            img.setRGB(i, j, texture.getRGB((resTextures.vec(0) * texture.getWidth).toInt, ((1 - resTextures.vec(1)) * texture.getHeight()).toInt));
          }
        }catch {
          case e: Exception => print("hehe")
        }
      }
    }
  }

  def getCubeVertices() : Array[Vector] = {
    var vertices = new Array[Vector](36)

    vertices(0) = new Vector(0.0,0.0,0.0)
    vertices(1) = new Vector(0.0,1.0,0.0)
    vertices(2) = new Vector(1.0,1.0,0.0)

    vertices(3) = new Vector(0.0,0.0,0.0)
    vertices(4) = new Vector(1.0,1.0,0.0)
    vertices(5) = new Vector(1.0,0.0,0.0)

    vertices(6) = new Vector(1.0,0.0,0.0)
    vertices(7) = new Vector(1.0,1.0,0.0)
    vertices(8) = new Vector(1.0,1.0,1.0)

    vertices(9) = new Vector(1.0,0.0,0.0)
    vertices(10) = new Vector(1.0,1.0,1.0)
    vertices(11) = new Vector(1.0,0.0,1.0)

    vertices(12) = new Vector(1.0,0.0,1.0)
    vertices(13) = new Vector(1.0,1.0,1.0)
    vertices(14) = new Vector(0.0,1.0,1.0)

    vertices(15) = new Vector(1.0,0.0,1.0)
    vertices(16) = new Vector(0.0,1.0,1.0)
    vertices(17) = new Vector(0.0,0.0,1.0)

    vertices(18) = new Vector(0.0,0.0,1.0)
    vertices(19) = new Vector(0.0,1.0,1.0)
    vertices(20) = new Vector(0.0,1.0,0.0)

    vertices(21) = new Vector(0.0,0.0,1.0)
    vertices(22) = new Vector(0.0,1.0,0.0)
    vertices(23) = new Vector(0.0,0.0,0.0)

    vertices(24) = new Vector(0.0,1.0,0.0)
    vertices(25) = new Vector(0.0,1.0,1.0)
    vertices(26) = new Vector(1.0,1.0,1.0)

    vertices(27) = new Vector(0.0,1.0,0.0)
    vertices(28) = new Vector(1.0,1.0,1.0)
    vertices(29) = new Vector(1.0,1.0,0.0)

    vertices(30) = new Vector(1.0,0.0,1.0)
    vertices(31) = new Vector(0.0,0.0,1.0)
    vertices(32) = new Vector(0.0,0.0,0.0)

    vertices(33) = new Vector(1.0,0.0,1.0)
    vertices(34) = new Vector(0.0,0.0,0.0)
    vertices(35) = new Vector(1.0,0.0,0.0)
    return vertices
  }

  def renderCube(img: BufferedImage,w:Int,h:Int,texture:BufferedImage) : Unit ={
    var projMat:Matrix = Matrix.createProjectionMatrix(0.1,1000,Math.PI / 2,h.toDouble / w.toDouble)
    var vertices = Main.model
    for(i <- 0 until Main.w){
      zBuffer(i) = new Array[Double](Main.h);
      for(j <- 0 until Main.h){
        zBuffer(i)(j) = 100000000;
      }
    }
    for(i <- 0 until vertices.length / 3){

      var translateVector:Vector = new Vector(0.0,200.0,900.0)
      var firstI:Vector =  Matrix.createRotationMatrix3DZ(Math.PI).multiplyVectorOnMatrix(Matrix.createRotationMatrix3DY(alpha).multiplyVectorOnMatrix(vertices(3*i))).plus(translateVector)
      var secondI:Vector = Matrix.createRotationMatrix3DZ(Math.PI).multiplyVectorOnMatrix(Matrix.createRotationMatrix3DY(alpha).multiplyVectorOnMatrix(vertices(3*i+1))).plus(translateVector)
      var thirdI:Vector = Matrix.createRotationMatrix3DZ(Math.PI).multiplyVectorOnMatrix(Matrix.createRotationMatrix3DY(alpha).multiplyVectorOnMatrix(vertices(3*i+2))).plus(translateVector)

      var line1:Vector = secondI.minus(firstI);
      var line2:Vector = thirdI.minus(firstI);
      var normal:Vector = line1.crossProduct3D(line2).normalize()

      if(Main.modelNormals.length == Main.model.length){
      //  normal = Main.modelNormals(3*i)
      }

      if(normal.dotProduct(firstI.minus(cameraPos)) <= 0.0) {

        var light = normal.dotProduct(lightPos).toFloat

        var first = projMat.multiplyVectorOnMatrix(new Vector(firstI.vec(0),firstI.vec(1),firstI.vec(2),0.0));
        var second = projMat.multiplyVectorOnMatrix(new Vector(secondI.vec(0),secondI.vec(1),secondI.vec(2),0.0));
        var third = projMat.multiplyVectorOnMatrix(new Vector(thirdI.vec(0),thirdI.vec(1),thirdI.vec(2),0.0));
        if (first.vec(3) != 0.0) {
          first = first.scalarMultiply(1.0 / first.vec(3))
        }
        first = first.plus(new Vector(1.0, 1.0, 1.0, 1.0))
        first = new Vector(first.vec(0) * w * 0.5, first.vec(1) * h * 0.5)
        if (second.vec(3) != 0.0) {
          second = second.scalarMultiply(1.0 / second.vec(3))
        }
        second = second.plus(new Vector(1.0, 1.0, 1.0, 1.0))
        second = new Vector(second.vec(0) * w * 0.5, second.vec(1) * h * 0.5)
        if (third.vec(3) != 0.0) {
          third = third.scalarMultiply(1.0 / third.vec(3))
        }
        third = third.plus(new Vector(1.0, 1.0, 1.0, 1.0))
        third = new Vector(third.vec(0) * w.toDouble * 0.5, third.vec(1) * h.toDouble * 0.5)
        //renderTriangle(img,first,second,third,new Color(0.0f,(1.0f * Math.abs(light)).min(1.0f).max(0.0),0.0f))
        println(Main.modelTextures(3*i).vec(0) + " " + Main.modelTextures(3*i+1).vec(0))
        renderTriangle(img,first,second,third,texture,Array(Main.modelTextures(3*i),Main.modelTextures(3*i+1),Main.modelTextures(3*i+2)),Array(firstI,secondI,thirdI))
       /* renderLine(img, first.vec(0).toInt, first.vec(1).toInt, second.vec(0).toInt, first.vec(1).toInt, Color.BLUE)
        renderLine(img, second.vec(0).toInt, second.vec(1).toInt, third.vec(0).toInt, third.vec(1).toInt, Color.BLUE)
        renderLine(img, third.vec(0).toInt, third.vec(1).toInt, first.vec(0).toInt, first.vec(1).toInt, Color.BLUE)*/
      }
    }
  }


}
