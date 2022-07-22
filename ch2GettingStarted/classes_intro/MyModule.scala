// A comment!
/* Another comment */
/** A documentation comment */

object MyModule {
  def abs(n: Int): Int = {
    if (n < 0) -n else n
  }

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  // Tail Recursive
  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n-1, n*acc)
    }
    go(n, 1)
  }

  // Fibonacci numbers
  def fib(n: Int): Int = {
    if (n == 0) then 0
    else if (n == 1) then 1
    else fib(n-2) + fib(n-1)
  }

  def main(args: Array[String]): Unit = {
    val nm = "Fib"
    println(formatResult(nm, 7, fib))
    //println(formatResult("Factorial", 5, factorial))
  }
}