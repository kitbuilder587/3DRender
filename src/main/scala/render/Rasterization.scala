package render

object Rasterization {
  def isPointInTriangle(A: (Double, Double), B: (Double, Double), C: (Double, Double), P: (Double, Double)): Boolean = {
    if(A._2 == C._2 && A._2 == B._2){
      return false;
    }else{
      if(A._2 == C._2) {
        return isPointInTriangle(B, A, C, P)
      }
    }

    val w1:Double = (A._1 * (C._2 - A._2) + (P._2 - A._2) * (C._1 - A._1) - P._1 * (C._2 - A._2)) / ((B._2-A._2)*(C._1 - A._1) - (B._1 - A._1) * (C._2 - A._2));
    val w2:Double = (P._2 - A._2 - w1 * (B._2 - A._2)) / (C._2 - A._2);
    return w1 >= 0 && w2 >= 0 && (w1+w2) <= 1;
  }

  def baracentricCoardinates(A: (Double, Double), B: (Double, Double), C: (Double, Double), P: (Double, Double)): Vector = {
    if(A._2 == C._2 && A._2 == B._2){
      return new Vector(0,0);
    }else{
      if(A._2 == C._2) {
        return baracentricCoardinates(B, A, C, P)
      }
    }

    val w1:Double = (A._1 * (C._2 - A._2) + (P._2 - A._2) * (C._1 - A._1) - P._1 * (C._2 - A._2)) / ((B._2-A._2)*(C._1 - A._1) - (B._1 - A._1) * (C._2 - A._2));
    val w2:Double = (P._2 - A._2 - w1 * (B._2 - A._2)) / (C._2 - A._2);
    return new Vector(w1,w2);
  }
}
