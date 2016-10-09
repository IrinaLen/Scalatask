/**
  * Created by irik on 29.09.16.
  */

object Manager {

  type fn = () => Unit
  val maxsize = 499
  val sizeofarray = 10

  class term(priority :Int, application: fn){
    val prior: Int = priority
    val app = application
  }

  def generate() : List[term]= {
    val app = new Array[fn](sizeofarray)
    val rnd = new scala.util.Random

    for {
      i <- 0 until sizeofarray
    } {
      app(i) = () => print("app" + i + "\n")
    }

    def applistcreate (total: List[term],len: Int): List[term] = {
      if (len == 0) total
      else  {
        val x = new term(rnd.nextInt(maxsize),
          app(rnd.nextInt(sizeofarray)))
        print(x.prior)
        print(" ")
        x.app()
        print("\n")

        applistcreate(x :: total, len - 1 )
      }
    }

    applistcreate(Nil, 5)

  }

  def FIFO (l: List[term]): List[term] = {
    l.reverse
  }

  def priority(l: List[term], high: Boolean): List[term] = {

    def createnums (list: List[term]) :List[Int] = {
      if (list.isEmpty) Nil
      else list.head.prior :: createnums(list.tail)
    }

      def forallnums (intarr: Array[Int], count: Int, total: List[term]): List[term] = {
        val arr = l.toArray
        if (count == l.length) total
        else {
          if (!high) {
            val n = intarr.indexOf(intarr.max)
            val intarr1 = intarr
            intarr1(n) = -1
            forallnums(intarr1, count + 1, arr(n)::total)
          } else{
            val n = intarr.indexOf(intarr.min)
            val intarr1 = intarr
            intarr1(n) = maxsize + 10
            forallnums(intarr1, count + 1, arr(n)::total)
          }
        }
      }

    forallnums(createnums(l).toArray,0,Nil)
  }



  def forall (list : List[term]): Unit = {
    if (!list.isEmpty) {
      val h = list.head
      print(h.prior + " ")
      h.app()
      forall(list.tail)
    }
  }



  def main(args: Array[String]): Unit = {
    val apps = generate()
  println("----------------")
    val polit :Int = 2
    // 0 - lasi in first out
    // 1 - first in first out
    // 2 - high priority
    // 3 - low priority

    polit match {
      case 0     =>  forall(apps)
      case 1     =>  forall(FIFO(apps))
      case 2     =>  forall(priority(apps, true))
      case 3     =>  forall(priority(apps, false))
      case whoa  => print("Undefined politic")
    }
  }
}
