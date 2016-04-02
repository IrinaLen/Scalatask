/**
  * Created by irik on 10.03.16.
  */
object filt {
  def main(args: Array[String]) {
    def srt(ls: List[Int]): List[Int] = {
      if (ls.isEmpty) List()
      else if (ls.head <= 45 && (ls.head % 7) == 0) ls.head::srt(ls.tail)
      else srt(ls.tail)
    }
  }

}
