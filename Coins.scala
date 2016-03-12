/**
  * Created by irik on 10.03.16.
  */
object Coins {
  def main(args: Array[String]) {
    def countChange(money: Int, coins: List[Int]): Int = {
      if ((coins.isEmpty) || (money < 0)) 0
      else if (money == 0) 1
      else (countChange(money, coins.init) + countChange(money - coins.last, coins))
    }
    countChange(19, List(1, 2, 3, 7, 10))
  }
}
