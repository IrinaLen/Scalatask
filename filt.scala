/**
  * Created by irik on 10.03.16.
  */
object filt {
  def main(args: Array[String]) {
    def srt(ls: List[Int]): List[Int] = {
      def sort(l: List[Int], tot: List[Int]): List[Int] = {
        if (l.isEmpty) tot
        else {
          if (l.last <= 45 && (l.last % 7) == 0) sort(l.init,l.last::tot)
          else sort(l.init, tot)
        }
      }
      sort(ls, List())
    }
    srt(List(7, 9, 0, 4, 21))
  }

}
