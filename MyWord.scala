/**
  * Created by irik on 15.04.16.
  */
abstract class  ParagraphElement{
}

class Word (elem: String) extends ParagraphElement{
}

class Space (w: Int) extends ParagraphElement{
}

object MyWord {
  def main(args: Array[String]) {

    def par (words: List[String], scr: Int, alig: String): List[List[ParagraphElement]] = {

      def sort (wr: List[String], ost: Int):List[String] = {
        if (wr.isEmpty) Nil
        else {
          val el = wr.head
          val lg: Int = el.length()
          require(lg <= scr, "So long word")
          if (lg <= ost){
            if (ost-lg == 0){
              el::Nil
            }
            else el :: sort(wr.tail, ost - lg - 1)
          }
          else Nil
        }
      }

      def forall (lst: List[String]):List[List[String]] = {
        if (lst.isEmpty) Nil
        else {
          val p = sort(lst, scr)
          p::forall(lst drop p.length)
        }
      }
      //alight
      val allsort = forall(words)

      def forall_alig (p: List[String] => List[ParagraphElement],l:List[List[String]]): List[List[ParagraphElement]] = {
        if (l.isEmpty) Nil
        else p(l.head)::forall_alig(p,l.tail)
      }

      def cases (lists: List[String]): List[ParagraphElement] = {
        if (lists.tail.isEmpty) new Word(lists.head):: Nil
        else (new Word(lists.head)::new Space(1)::cases(lists.tail))
      }

      def casLeft (lists: List[String], over: Int): List[ParagraphElement] = {
        if (lists.tail.isEmpty) new Word(lists.head)::new Space(over):: Nil
        else (new Word(lists.head)::new Space(1)::casLeft(lists.tail, over))
      }

      def right (ls: List[String]): List[ParagraphElement] = {
        val over = scr - ls.length
        if (over == 0) cases(ls)
        else (new Space(over) :: cases(ls))
      }

      def left (ls: List[String]): List[ParagraphElement] = {
        val over = scr - ls.length

        if (over == 0) cases(ls)
        else casLeft(ls, over)

      }
      def centre (ls: List[String]): List[ParagraphElement] = {
        val over: Int = scr - ls.length

        if (over == 0) cases(ls)
        else new Space(over/2) :: casLeft(ls, (over - over / 2))

      }

      def fit (ls: List[String]): List[ParagraphElement] = {
        val over = scr - ls.length
        val sp: Int = over / (ls.length - 1)
        val last : Int = over % (ls.length - 1)

        def casFit (lsts: List[String]): List[ParagraphElement] = {
          if (lsts.tail.length == 2) (new Word(lsts.head)::new Space(last) :: new Word(lsts.tail.head):: Nil)
          else (new Word(lsts.head)::new Space(sp):: casFit(lsts.tail))
        }

        if (over == 0) cases(ls)
        else casFit(ls)

      }

      if (alig == "r") forall_alig(right, allsort)

      else if (alig == "l") forall_alig(left, allsort)

      else if (alig == "f") forall_alig(fit, allsort)

      else if (alig == "c") forall_alig(centre, allsort)

      else Nil
    }

    par (List("aa","a","aj", "a"), 5, "r")

  }
}
