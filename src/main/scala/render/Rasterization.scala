package render

object Rasterization {
  def isPointInTriangle(A: (Int, Int), B: (Int, Int), C: (Int, Int), P: (Int, Int)): Boolean = {
    var AB = (B._1 - A._1, B._2 - A._2);
    var AC = (C._1 - A._1, C._2 - A._2);
    var AP = (P._1 - A._1, P._2 - A._2);
    var X = (AB._1, AC._1, AP._1);
    var Y = (AB._2, AC._2, AP._2);
    var mul = (X._2 * Y._3 - X._3 * Y._2, X._3 * Y._1 - X._1 * Y._3, X._1 * Y._2 - X._2 * Y._1)
    var u = mul._1.toFloat / mul._3.toFloat
    var v = mul._2.toFloat / mul._3.toFloat
    if (u + v <= 1 && u >= 0 && v >= 0) {
      return true;
    } else {
      return false;
    }
  }
}
