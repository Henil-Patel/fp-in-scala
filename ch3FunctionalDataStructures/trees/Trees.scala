sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /*
  * Exercise 25: Write a function size that counts the number of nodes (leaves and branches)
  * in a tree.
  * */

  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l ,r) => 1 + size(l) + size(r)
    }
  }

  /*
  * Exercise 26: Write a function maximum that returns the maximum element in a Tree[Int].
  * (Note: In Scala, you can use x.max(y) or x max y to compute the maximum of two integers
  * x and y.)
  * */

  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(x) => x
      case Branch(l, r) =>
        if (maximum(l) > maximum(r)) maximum(l)
        else maximum(r)
    }
  }

  /*
  * Exercise 27: Write a function depth that returns the maximum path length from the root
  * of a tree to any leaf.
  * */

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + depth(l).max(depth(r))
    }
  }

  /*
  * Exercise 28: Write a function map, analogous to the method of the same name on List,
  * that modifies each element in a tree with a given function.
  * */

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  /*
  * Exercise 29: Generalize size, maximum, depth, and map, writing a new function fold
  * that abstracts over their similarities. Reimplement them in terms of this more general
  * function. Can you draw an analogy between this fold function and the left and right folds
  * for List?
  * */

  def fold[A, B](t: Tree[A], z: A => B, f: (B, B) => B): B = {
    t match {
      case Leaf(x) => z(x)
      case Branch(l, r) => f(fold(l, z, f), fold(r, z, f))
    }
  }

  def sizeFold[A](t: Tree[A]): Int = {
    fold(t, (x: A) => 1, (l: Int, r: Int) => 1 + l + r)
  }

  def maximumFold(t: Tree[Int]): Int = {
    fold(t, (x: Int) => x, (l: Int, r: Int) => if (l > r) l else r)
  }

  def depthFold[A](t: Tree[A]): Int = {
    fold(t, (x: A) => 0, (l: Int, r: Int) => 1 + l.max(r))
  }

  def mapFold[A, B](t: Tree[A], f: A => B): Tree[B] = {
    fold(t, (x: A) => Leaf(f(x)), (a: Tree[B], b: Tree[B]) => Branch(a, b))
  }
}

@main
def mainFunc(): Unit = {
  val a = Branch(Branch(Branch(Leaf(-1), Branch(Leaf(4), Leaf(1))), Leaf(-3)), Leaf(-2))
  println("Size: " + Tree.size(a))
  println("Maximum: " + Tree.maximum(a))
  println("Depth: " + Tree.depth(a))
  println("Map: " + Tree.map(a)(x => x * -1))
  println("------------------------------------")
  println("Size via Fold: " + Tree.sizeFold(a))
  println("Maximum via Fold: " + Tree.maximumFold(a))
  println("Depth via Fold: " + Tree.depthFold(a))
  println("Map via Fold: " + Tree.mapFold(a, x => x + 5))
}
