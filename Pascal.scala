/**
  * Created by irik on 09.03.16.
  */
object Pascal {
  def main(args: Array[String]) {
    def pascal(c: Int, r: Int): Int = {
      if (r < c) -1
      else if (r == c || c == 0) 1
      else pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
    pascal (2, 10)
  }
}
