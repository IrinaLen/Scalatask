/**
  * Created by irik on 09.03.16.
  */
object totheend {
  def main(args: Array[String]) {
    def inend(l: List[Any], a: Any): List[Any] = {
      def add(ls: List[Any], el: List[Any]): List[Any] = {
        if (ls.isEmpty) el:::a::Nil
        else add(ls.init, ls.last::el)
      }
      add(l, List())
    }
    inend(List(1, 2, "Len"), "Ira")
  }

}
