/**
  * Created by irik on 09.03.16.
  */
object SumofElemrnts {
  def main(args: Array[String]) {
    def sum(l: List[Double]): Double = {
      def elsum(ls: List[Double], sm: Double): Double = {
        if (ls.isEmpty) sm
        else elsum(ls.tail,sm + ls.head)
      }
      elsum(l, 0)
    }
    sum(List(1, 2, 4,7.9, 1e-7, 3e4))
  }
}
