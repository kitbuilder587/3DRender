package render

import render.Vector
import Matrix._

class Matrix (var mat: Array[Array[Double]]) {
  var m = mat.length
  var n = mat(0).length

  

  @throws[Exception]
  def multiplyMatrixOnVector(v: Vector):Vector = {
    val vMat = createMatrixFromVector(v)
    val resMat = this.multiply(vMat.getTransposedMatrix())
    val res = new Vector(new Array[Double](v.n))
    for (i <- 0 until v.n) {
      res.vec(i) = resMat.mat(i)(0)
    }
    res
  }

  @throws[Exception]
  def multiplyVectorOnMatrix(v: Vector):Vector = {
    val vMat = createMatrixFromVector(v)
    val resMat = vMat.multiply(this)
    val res = new Vector(new Array[Double](v.n))
    for (i <- 0 until v.n) {
      res.vec(i) = resMat.mat(0)(i)
    }
    res
  }

  def createMatrixFromVector(v: Vector):Matrix = {
    val res = getNullMatrix(1, v.n)
    for (i <- 0 until v.n) {
      res.mat(0)(i) = v.vec(i)
    }
    res
  }

  def getTransposedMatrix():Matrix = {
    val res:Array[Array[Double]] = new Array[Array[Double]](n)
    for(i <- 0 until n){
      res(i) = new Array[Double](m)
      for(j <- 0 until m){
        res(i)(j) = mat(j)(i)
      }
    }
    return new Matrix(res)
  }

  @throws[Exception]
  def multiply(b: Matrix):Matrix= {
    if (n == b.m) {
      val c: Matrix = getNullMatrix(m, b.n)
      for (i <- 0 until m) {
        for (j <- 0 until b.n) {
          for (k <- 0 until n) {
            c.mat(i)(j) += mat(i)(k) * b.mat(k)(j)
          }
        }
      }
      return c
    }
    else throw new Exception("Different mat sizes")
  }



}
object Matrix{

  def getNullMatrix(m: Int, n: Int):Matrix = {
    val mat = new Array[Array[Double]](m)
    for (i <- 0 until m) {
      mat(i) = new Array[Double](n)
      for (j <- 0 until n) {
        mat(i)(j) = 0
      }
    }
    return new Matrix(mat)
  }
  
  def createProjectionMatrix(near: Double, far: Double, fov: Double, aspectRatio: Double) = {
    var fovR = 0.5 * fov
    val tanCalc = 1.0 / Math.tan(fovR)
    val matProj = getNullMatrix(4, 4)
    matProj.mat(0)(0) = aspectRatio * tanCalc
    matProj.mat(1)(1) = fovR
    matProj.mat(2)(2) = far / (far - near)
    matProj.mat(3)(2) = (-far * near) / (far - near)
    matProj.mat(2)(3) = 1
    matProj
  }

  def createRotationMatrix3DX(phi:Double) = {
    val matRot = getNullMatrix(3, 3)
    matRot.mat(0)(0) = 1
    matRot.mat(1)(1) = Math.cos(phi)
    matRot.mat(1)(2) = -Math.sin(phi)
    matRot.mat(2)(1) = Math.sin(phi)
    matRot.mat(2)(2) = Math.cos(phi)
    matRot
  }

  def createRotationMatrix3DY(phi:Double) = {
    val matRot = getNullMatrix(3, 3)
    matRot.mat(0)(0) = Math.cos(phi)
    matRot.mat(1)(1) = 1
    matRot.mat(2)(0) = -Math.sin(phi)
    matRot.mat(0)(2) = Math.sin(phi)
    matRot.mat(2)(2) = Math.cos(phi)
    matRot
  }
  def createRotationMatrix3DZ(phi:Double) = {
    val matRot = getNullMatrix(3, 3)
    matRot.mat(0)(0) = Math.cos(phi)
    matRot.mat(2)(2) = 1
    matRot.mat(0)(1) = -Math.sin(phi)
    matRot.mat(1)(0) = Math.sin(phi)
    matRot.mat(1)(1) = Math.cos(phi)
    matRot
  }
  def createScaleMatrix3D(s:Vector) = {
    val matRot = getNullMatrix(3, 3)
    matRot.mat(0)(0) = s.vec(0)
    matRot.mat(1)(1) = s.vec(1)
    matRot.mat(2)(2) = s.vec(2)
    matRot
  }
}
