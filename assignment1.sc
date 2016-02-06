package rec_pckg

object assignment1 {
// EXERCISE 1
	// Eli tää ottaa ton sarakkeen ja rivin. Se määritelmän mukaan on summa niistä
	// "päällä" olevista alkioista, eli ainakin rivi - 1. Ja sarakkeet on -1 ja +1
	def pascal(col: Int, row: Int): Int =
		if ( row == 0 && col == 0) 1
		else if (col < 0 || col > row) 0
		else pascal(col - 1, row - 1) + pascal(col, row - 1)
	
	pascal(0,0)
	pascal(0,1)
	pascal(1,1)
	pascal(2,4)
	pascal(1,4)
	pascal(3,4)


// EXERCISE 2

	def balance(chars: List[Char]): Boolean = {
		def balance_rec(chars: List[Char], bal_count: Int = 0): Boolean =
			// Lopetusehto
			if (chars.isEmpty && bal_count == 0) true
			// Jos count menee negatiiviseksi, on balanssi vituillaan. Samoin jos tässä ei enää ole
			// merkkejä jäljellä, se tarkoittaa että count oli lopussa ei-nolla joten palautetaan false.
			else if (bal_count < 0 || chars.isEmpty) false
			// Rekursiot eri tilanteissa:
			else if ( chars.head == '(' ) balance_rec(chars.tail, bal_count + 1)
			else if ( chars.head == ')' ) balance_rec(chars.tail, bal_count - 1)
			else balance_rec(chars.tail, bal_count)
		
		balance_rec(chars, 0)
			
	
	
	}
	balance("(test(test(t)))(sd)".toList)
	balance(":-)".toList)
	balance("())(".toList)

// EXERCISE 3

	
	def countChange(money:Int, coins: List[Int]): Int =
		// Pysäytysehto, case "ok"
		if (money == 0) 1
		// Pysäytysehto, case "not ok"
		else if (money > 0 && coins.isEmpty) 0
		// Jos jäljellä olevaa summaa voidaan pienentää nykyisellä kolikolla, niin haaraudutaan
		// kahteen osaa, joista toisessa se valitaan ja toisessa ei.
		else if (money - coins.head >= 0) countOnSorted(money - coins.head, coins) +
		countOnSorted(money, coins.tail)
		// Jos ei voida valita, niin mennään seuraavaan kolikkoon.
		else countOnSorted(money, coins.tail)
	
	countChange(7, List(2,1))
	
}