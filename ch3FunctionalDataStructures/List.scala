//package fpinscala.datastructures

/*
* (1)
*     - Datatypes are introduced with the keyword trait
*     - It is an abstract interface which may *optionally* contain implementations of methods
*     - List is declared with no methods on it
*     - Adding sealed means that all implementations of the trait must be delcared in this file
* */

sealed trait List[+A] // (1)

/*
* (2)
*     - First implementation / data constructor of List
*     - Introduced with 'case' keyword
*     - Nil data constructor indicates list is empty
* */

case object Nil extends List[Nothing] // (2)

/*
* (3)
*     - Second implementation / data constructor of List
*     - Also introduced with 'case' keyword
*     - Cons indicates nonempty list (short for construct)
*     - Nonempty list contains an initial element (head), followed by a List of remaining elements (tail)
*     - Data types can also be polymorphic as denoted by [+A]
*         - Where '+' indicates type parameter A is covariant
*             - Covariant - for all types X & Y if X is a subtype of Y then
*                           List[X] is also a subtype of List[Y]
*               e.g 'Nothing' is a subtype of Int, Double, String etc. hence
*                   List[Nothing] or Nil is a subtype of List[Int], List[Double], List[String] etc.
* */

case class Cons[+A](head: A, tail: List[A]) extends List[A] // (3)


/*
* (4)
*     - Also called 'companion object' to List (trait)
*     - This is just an object with the same name as the data type
*         - Used for keeping convenience functions for creating or working with values of the data type
* */
object List { // (4)
  /*
  * (5)
  *     - This is the first example of pattern matching
  *     - Matching begins with an expression 'ints match {...}' called the target or scrutinee
  *     - The left of the '=>' is called the 'case'
  *     - The right of the '=>' is called the 'result'
  *     - First 'arm' checks for whether if ints matches with an empty list (Nil)
  *     - Second 'arm' checks for whether if ints matches with a non-empty list (Cons)
  *         - Here, add head (first element) with recursive sum of tail (rest of elements)
  *     - Since unit in sum is 0, Nil case returns 0 (i.e nothing to add) otherwise, return sum
  *     - Note that the return type in signature matches the type of result in arms (Int)
  * */
  def sum(ints: List[Int]): Int = ints match { // (5)
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  /*
  * (6)
  *     - This is the second example of pattern matching
  *     - Matching begins with an expression 'ds match {...}' called the target or scrutinee
  *     - The left of the '=>' is called the 'case'
  *     - The right of the '=>' is called the 'result'
  *     - First arm returns unit of multiplication operator, 1
  *         - If list matches with empty list (Nil) empty return 1
  *     - Second arm returns 0 if the head is 0 (it is inconsequential what the rest of the elements are)
  *         - '_' denotes placeholder
  *     - Third arm returns a recursively called product of head and tail
  *     - Note that the return type in signature matches the type of result in arms (Double)
  * */
  def product(ds: List[Double]): Double = ds match { // (6)
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  /*
  * Summary of pattern matching:
  *   A pattern matches the target if there exists an assignment of variables in the pattern
  *   to subexpressions of the target that make it structurally equivalent to the target.
  * */


  /*
  * (7)
  *     - The function apply is a variadic function
  *         - This means that it accepts 0 or more arguments of type A
  *         - Denoted by *
  *     - Common idiom to have apply function in companion object
  *         - Used for constructing instances of data type
  *     - Variadic functions provide syntax sugar for creating and passing a Seq of elements explicitly
  *     - Apply function checks if 'as' (Seq) is empty
  *         - If it is, return Nil (List[Nothing])
  *         - Else, return Cons (head, List[tail]) with recursive call to itself generate tail
  * */

  def apply[A](as: A*): List[A] = // (7)
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /*
  * Exercise 2: Implement the function tail for removing the first element of a list.
  * Notice the function takes constant time. What are the different choices you could make
  * in your imlpementation if the List is Nil?
  * */
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, xs) => xs
  }

  /*
  * Exercise 3: Using the same idea, implement the function setHead for replacing the first
  * element of a List with a different value.
  * */
  def setHead[A](as: List[A], v: A): List[A] = as match {
    case Nil => Cons(v, Nil)
    case Cons(_, xs) => Cons(v, xs)
  }

  /*
  * Exercise 4: Generalize tail to the function drop, which removes the first n elements
  * from a list. Notice this function takes time proportional only to the number of elements
  * being dropped - we do not need to make a copy of the entire List.
  * */
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 1) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  /*
  * Exercise 5: Implement dropWhile, which removes elements from the List prefix as long
  * as they match a predicate.
  * Note the 'if guard' in the 2nd case. This helps with additional checks.
  * */
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    // Note currying in line 145 => dropWhile(xs)(f)
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  /*
  * Append Function
  * */
  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(x, xs) => Cons(x, append(xs, a2))
  }

  /*
  * Exercise 6: Implement a function init, which returns a List consisting of all
  * but the last element of a List. E.g given List(1,2,3,4), init will return List(1,2,3).
  * */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  /*
  * It is also possible to abstract away the operators and Nil matches.
  * Consider the 'sum' and 'product' methods.
  * They are effectively common except for the + and * operators and
  * unit values 0 (for sum) and 1.0 (for product). Consider the following approach.
  * */
  def foldRight[A,B](l: List[A], z: B)(f: (A,B) => B): B = l match {
    case Nil => z                                         // 'z' indicates the unit value
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))         // apply f
  }

  /*
  * New implementations of sum and product using foldRight
  * */
  def sumRight(l: List[Int]): Int = foldRight(l, 0)((x,y) => x + y)

  def productRight(l: List[Double]): Double = foldRight(l, 1.0)(_ * _)  // _ * _ is more concise notion for
                                                                    // (x,y) => x * y

  /*
  * foldRight evaluation trace
  * foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)((x, y) => x + y)
  * 1 + foldRight(Cons(2, Cons(3, Nil)), 0)((x, y) => x + y)
  * 1 + (2 + foldRight(Cons(3, Nil), 0)((x, y) => x + y))
  * 1 + (2 + (3 + (foldRight(Nil, 0)((x, y) => x + y))))
  * 1 + (2 + (3 + (0)))
  * 6
  * */

  /*
  * Exercise 9: Compute the length of a list using foldRight.
  * */
  def lengthRight[A](l: List[A]): Int = foldRight(l, 0)((_, x) => x + 1)

  /*
  * Exercise 10: foldRight is not tail-recursive and will StackOverflow
  * for large lists. Convince yourself that this is the case, then write
  * another general list-recursion function, foldLeft that this is
  * tail-recursive, using the techniques we discussed in the previous chapter
  * */
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(foldLeft(xs, z)(f), x)
  }

  /*
  * Exercise 11: Write sum, product, and a function to compute the length of a
  * list using foldLeft.*/
  def sumLeft(l: List[Int]): Int = foldLeft(l, 0)((x, y) => x + y)
  def productLeft(l: List[Double]): Double = foldLeft(l, 1.0)((x, y) => x * y)
  def lengthLeft[A](l: List[A]): Int = foldLeft(l, 0)((x, _) => x + 1)

  /*
  * Exercise 12: Write a function that returns the reverse of a list (so given
  * List(1,2,3) it returns List(3,2,1). See if you can write it using a fold.
  * */
  def reverse[A](l: List[A]): List[A] = l match {
    case Cons(x, xs) => append(reverse(xs), List(x))
    case Nil => Nil
  }
  // revisit later
  //def revFold[A](l: List[A]): List[A] = foldLeft(l, List[A]())((x, y) => Cons(y, x))

  /*
  * Exercise 13 (hard): Can you write foldLeft in terms of foldRight? Vice versa?
  * */

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(l, (b: B) => b)((a: A, g: B => B) => (b: B) => g(f(b, a)))(z)
  }

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(l, (b: B) => b)((g: B => B, a: A) => (b: B) => g(f(a, b)))(z)
  }

  /*
  * Exercise 14: Implement append in terms of either foldLeft or foldRight.
  * */
  def appendViaFold[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((x, y) => Cons(x, y))
  }

  /*
  * Exercise 15: Write a function that concatenates a list of lists into a single list.
  * Its runtime should be linear in the total length of all lists. Try to use functions
  * we have already defined.
  * */
  def concatList[A](ls: List[List[A]]): List[A] = {
    foldRight(ls, Nil: List[A])(append)
  }

  /*
  * Exercise 16: Write a function that transforms a list of integers by adding 1
  * to each element. (Reminder: this should be a pure function that returns a new
  * List!)
  * */
  def addOne(ls: List[Int]): List[Int] = ls match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, addOne(xs))
  }

  // Alternative implementation using fold
  def incrementEach(ls: List[Int]): List[Int] = {
    foldRight(ls, Nil: List[Int])((x, xs) => Cons(x + 1, xs))
  }

  /*
  * Exercise 17: Write a function that turns each value in a List[Double] into
  * a String.
  * */
  def doubleToString(ls: List[Double]): List[String] = {
    foldRight(ls, Nil: List[String])((x, xs) => Cons(x.toString, xs))
  }
  /*
  * Exercise 18: Write a function map, that generalizes modifying each element
  * in a list while maintaining the structure of the list.
  * */
  def map[A, B](l: List[A])(f: A => B): List[B] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }
  }

  // Alternatively using fold
  def mapFold[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((x, xs) => Cons(f(x), xs))
  }

  /*
  * Exercise 19: Write a function filter that removes elements from a list unless they satisfy
  * a given predicate. Use it to remove all odd numbers from a List[Int].
  * */
  def filter[A](ls: List[A])(p: A => Boolean): List[A] = ls match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (p(x)) Cons(x, filter(xs)(p))
      else xs
  }

  // Alternatively using fold
  def filterFold[A](ls: List[A])(p: A => Boolean): List[A] =
    foldRight(ls, Nil: List[A])((x, xs) => if (p(x)) Cons(x, xs) else xs)

  /*
  * Exercise 20: Write a function flatMap, that works like map except that the function
  * given will return a list instead of a single result, and that list should be inserted
  * into the final resulting list.*/

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concatList(map(l)(f))

}

@main
def mainFunc(): Unit = {
  val a = new Cons[Int](1, Cons(2, Cons(3, Cons(4, Nil))))
  val b = new Cons[Double](1.0, Cons(2.0, Cons(3.0, Cons(4.0, Nil))))
  println(a)
  println("---------------------------------------------")

  println(List.setHead(a, 5))
  println(List.tail(a))
  println(List.drop(a, 4))
  println(List.dropWhile(a)(b => b % 2 == 0))
  println(List.init(a))
  /*
  * Exercise 7: Can 'product' implemented using foldRight immediately halt recursion
  * and return 0.0 if it encounters a 0.0? Why or why not?
  * -- No because no case handles exiting with a 0.0. Therefore it would arrive at 0.0 via
  *    recursion cycles till it terminates with the exit condition (Nil).
  * */
  println("---------------------------------------------")
  println("Sum Default: " + List.sum(a))
  println("Product Default: " + List.product(b))
  val c = new Cons[Double](1.0, Cons(0.0, Cons(3.0, Cons(4.0, Nil))))
  println("Sum foldRight: " + List.sumRight(a))
  println("Product foldRight: " + List.productRight(c))
  println("Length of 'a' foldRight: " + List.lengthRight(a))
  println("Sum foldLeft: " + List.sumLeft(a))
  println("Product foldLeft: " + List.productLeft(c))
  println("Length of 'a' foldLeft: " + List.lengthLeft(a))
  println("---------------------------------------------")

  /*
  * Exercise 8: See what happens when you pass Nil and Cons to foldRight
  * e.g foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
  * What do you think this says about the relationship between foldRight and
  * the data constructors of List?
  * -- This shows that foldRight can be used to construct a singly-linked list by giving it
  *    Nil as the terminating condition and Cons definition for storing each element from List
  * */
  println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))) // Cons(1,Cons(2,Cons(3,Nil)))
  // This shows that foldRight can be used to construct a singly linked list.
  println("Reverse without fold: " + List.reverse(a))
  //println("Reverse with left fold: " + List.revFold(a))
  val d = new Cons[Int](5, Cons(6, Cons(7, Cons(8, Nil))))
  println("Append via fold: " + List.appendViaFold(a, d))
  val e = List(List(1, 2), List(9, 10), List(5, 7))
  println("Concatenate via fold: " + List.concatList(e))
  println("---------------------------------------------")
  println("Add one to each element: " + List.incrementEach(a))
  println("Double to String: " + List.doubleToString(b))
  println("Map: " + List.map(a)(x => x*2))
  println("Map with folding: " +  List.mapFold(a)(x => "[" + x.toString + "]"))
  println("Filter: " + )

}
