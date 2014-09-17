package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println(countChange(300,List(5,10,20,50,100,200,500)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c==0 || r==c) 1
    else pascal(c,r-1) + pascal(c-1,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
	def isBalanced(input: List[Char], stack:List[Char]): Boolean = {
		if (input.isEmpty) stack.isEmpty
		else if (input.head=='(') isBalanced(input.tail, input.head :: stack)
		else if (input.head==')') !stack.isEmpty && isBalanced(input.tail, stack.tail)
		else isBalanced(input.tail, stack);
	}
	isBalanced(chars, "".toList)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    var totalAmount = 0;
    
    def check(money: Int, coins: List[Int]) {
      if (coins.isEmpty) return
      if (money>coins.head) {
        check(money-coins.head, coins)
        check(money,coins.tail)
      }
      else if (money<coins.head) {
        check(money,coins.tail)
      }
      else if (money-coins.head == 0) {
        totalAmount += 1
      }
    }
    
    check(money,coins.sorted)
    totalAmount
  }
}
