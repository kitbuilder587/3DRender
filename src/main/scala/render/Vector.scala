package render

class Vector (val a: Array[Double]) {
  var n = a.length
  var vec = new Array[Double](n)
  for (i <- 0 until n) {
    vec(i) = a(i)
  }
  def this(x: Double,y:Double,z:Double,t:Double) = this(Array[Double](x,y,z,t))
  def this(x: Double,y:Double,z:Double) = this(Array[Double](x,y,z))
  def this(x: Double,y:Double) = this(Array[Double](x,y))
  @throws[Exception]
  def plus(b: Vector): Vector = if (n == b.n) {
    val res = new Vector(vec)
    for (i <- 0 until n) {
      res.vec(i) =res.vec(i) + b.vec(i)
    }
    res
  }
  else throw new Exception("Different vector sizes")

  @throws[Exception]
  def minus(b: Vector): Vector = if (n == b.n) {
    val res = new Vector(vec)
    for (i <- 0 until n) {
      res.vec(i) -= b.vec(i)
    }
    res
  }
  else throw new Exception("Different vector sizes")

  def scalarMultiply(constant: Double): Vector = {
    val res = new Vector(vec)
    for (i <- 0 until n) {
      res.vec(i) *= constant
    }
    res
  }

  def length(): Double = {
    var res = 0.0
    for (i <- 0 until n) {
      res += vec(i) * vec(i)
    }
    Math.sqrt(res)
  }

  def dotProduct(b: Vector): Double = {
    var res = 0.0
    for (i <- 0 until n.min(b.n)) {
      res += vec(i) * b.vec(i)
    }
    res
  }

  def crossProduct3D(b: Vector): Vector = {
    var res:Vector = new Vector(0.0,0.0,0.0);
    res.vec(0) = vec(1) * b.vec(2) - vec(2) * b.vec(1)
    res.vec(1) = vec(2) * b.vec(0) - vec(0) * b.vec(2)
    res.vec(2) = vec(0) * b.vec(1) - vec(1) * b.vec(0)
    res
  }
  
  def normalize(): Vector = {
    this.scalarMultiply(1.0 / this.length())
  }

}
