package rec_pckg

object assignment1 {
// EXERCISE 1
	// A function to determine values of the Pascal's triangle.
	
	def pascal(col: Int, row: Int): Int =
		if ( row == 0 && col == 0) 1
		else if (col < 0 || col > row) 0
		else pascal(col - 1, row - 1) + pascal(col, row - 1)
	
	// Testing 
	
	pascal(0,0)
	pascal(0,1)
	pascal(1,1)
	pascal(2,4)
	pascal(1,4)
	pascal(3,4)


// EXERCISE 2

	// A function to determine whether the parentheses in a string are balanced.

	def balance(chars: List[Char]): Boolean = {
		def balance_rec(chars: List[Char], bal_count: Int = 0): Boolean =
			// Stopping criterion for a passing case
			if (chars.isEmpty && bal_count == 0) true
			//  If the count becomes negative, the balance is off. Also, if there are no characters left
			// and the balance is not zero, it's off.
			else if (bal_count < 0 || chars.isEmpty) false
			// If an opening bracket is encountered, increment the count and vice versa. 
			else if ( chars.head == '(' ) balance_rec(chars.tail, bal_count + 1)
			else if ( chars.head == ')' ) balance_rec(chars.tail, bal_count - 1)
			// If any other character is encountered, just move on.
			else balance_rec(chars.tail, bal_count)
		
		balance_rec(chars, 0)
			
	}
	
	// Testing
	balance("(test(test(t)))(sd)".toList)
	balance(":-)".toList)
	balance("())(".toList)

// EXERCISE 3
	
	// Count the ways in which change can be given. The function takes the amount of money and 
	// list of unique, integer coin denominations (the order of which doesn't matter).
	
	def countChange(money:Int, coins: List[Int]): Int =
		// A "success" case
		if (money == 0) 1
		// A failed case
		else if (money > 0 && coins.isEmpty) 0
		// If the remaining sum can be reduced with the current coin, there are two ways to move
		// forward: Take the coin and check the same coin next time too, or just move to the next one without
		// taking it. Both of these are explored.
		else if (money - coins.head >= 0) countChange(money - coins.head, coins) +
		countChange(money, coins.tail)
		// If the current coin cannot be chosen, just move on.
		else countChange(money, coins.tail)
	
	countChange(7, List(2,1))
	
}
