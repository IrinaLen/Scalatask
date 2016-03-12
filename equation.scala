/**
  * Created by irik on 10.03.16.
  */
object equation {
  def main(args: Array[String]) {
    def equation(a: Double, b: Double, c: Double, d: Double): List[Double] = {
      def Tres(a: Double, b: Double, c: Double, d: Double): List[Double] = {
        if (d == 0) 0::Duo(a, b, c)
        else {
          val Q: Double= ((b * b) / (a * a) - (3 * c) / a) / 9
          val R: Double = (2 * Math.pow(b, 3) / Math.pow(a, 3) - (9 * b * c) / (a * a) + (27 * d) / a) / 54

          if ((Q * Q * Q - R * R) > 0) {
            val t: Double = Math.acos(R / Math.sqrt(Math.pow(Q, 3))) / 3.0
            (-2 * Math.sqrt(Q) * Math.cos(t) - b / (3 * a)) :: (-2 * Math.sqrt(Q) * Math.cos(t + (2 * Math.PI / 3))
              - b / (3 * a)) :: (-2 * Math.sqrt(Q) * Math.cos(t - (2 * Math.PI / 3)) - b / (3 * a)) :: Nil
          }
          else {
            val A: Double = - Math.signum(R) * Math.pow(Math.abs(R) + Math.sqrt(R * R - Q * Q * Q), 1.0 / 3.0)
            var B: Double = 0
            if (A != 0) B = Q / A
            if (A == B) -A - b / (3 * a) :: (A + B) - b / (3 * a) :: Nil
            else (A + B) - b / (3 * a) :: Nil
          }
        }
      }

      def Duo(a: Double, b: Double, c: Double): List[Double] = {
        val D = b * b - 4 * a * c
        if (D < 0) Nil
        else ((-b - Math.sqrt(D)) / (2 * a))::((-b + Math.sqrt(D)) / (2 * a))::Nil
      }

      def Uno(a: Double, b: Double): List[Double]= {
        if (a == 0 && b == 0) (1.0/ 0)::Nil
        else (-b / a)::Nil
      }

      if (a != 0) Tres(a, b, c, d)
      else if (b != 0) Duo(b, c, d)
      else Uno(c, d)
    }
    equation(1, 4, 3, -1)
  }
}
