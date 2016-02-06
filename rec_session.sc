package rec_pckg

// Two recursive implementations of the factorial function: conventional and tail-recursive.

object rec_session {
  // The "normal" recursive version.
  def factorial(n: Int):Int =
  	if (n == 0) 1 else n*factorial(n-1)       //> factorial: (n: Int)Int
  
  
  // Tail-recursive version: Function call has to be the last thing done.
  def factorial_tr(n: Int, current:Int = 1): Int =
  	if(n == 0) current else factorial_tr(n-1, current*n)
                                                  //> factorial_tr: (n: Int, current: Int)Int
  
  // Testing
  factorial_tr(2)                                 //> res0: Int = 2
  factorial_tr(3)                                 //> res1: Int = 6
  factorial_tr(4)                                 //> res2: Int = 24
  factorial_tr(5)                                 //> res3: Int = 120
  
  
  factorial(2)                                    //> res4: Int = 2
  factorial(3)                                    //> res5: Int = 6
  factorial(4)                                    //> res6: Int = 24
  factorial(5)                                    //> res7: Int = 120
  
}
