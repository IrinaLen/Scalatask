/**
  * Created by irik on 10.03.16.
  */
object filt2 {
  def main(args: Array[String]) {
    def fn1(x: Int): Boolean = {
      if (x > 10) true
      else false
    }

    def filt(ls: List[Int], fn: Int => Boolean): List[Int] = {
      def sort(l: List[Int], tot: List[Int]): List[Int] = {
        if (l.isEmpty) tot
        else {
          if (fn(l.head)) sort(l.tail,tot:::l.head::Nil)
          else sort(l.tail, tot)
        }
      }
      sort(ls, List())
    }

    filt(List(7, 9, 0, 4, 21, 290), fn1)
  }
}
