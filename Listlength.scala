/**
  * Created by irik on 09.03.16.
  */
object Listlength {
  def main(args: Array[String]) {
    def len(l: List[Any]):Int = {
      def ListLeng(ls: List[Any], res: Int): Int = {
        if (ls.isEmpty) res
        else ListLeng(ls.tail,res + 1)
      }
      ListLeng(l, 0)
    }

    len(List(1,22,1554,4,"i","rt"))
  }

}
