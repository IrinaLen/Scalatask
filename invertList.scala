/**
  * Created by irik on 09.03.16.
  */
object invertList {
  def main(args: Array[String]): Unit = {
    def invert(l: List[Any]):List[Any] = {
      def invlist(ls: List[Any], tot: List[Any]):List[Any]= {
        if (ls.isEmpty) tot
        else invlist(ls.tail,ls.head::tot)
      }
      invlist(l,List())
    }
    invert(List(1, 22, 1554, 4, "i", "rt"))
  }
}
