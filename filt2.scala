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
      if (ls.isEmpty) List()
      else if (fn(ls.head)) ls.head::filt(ls.tail, fn)
      else filt(ls.tail, fn)
    }

    filt(List(7, 9, 0, 4, 21, 290), fn1)
  }
}
