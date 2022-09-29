
import scala.{Option as _, Some as _, None as _}

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(x) => Some(f(x))
      case None => None
    }
  }

object Option:
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

@main
def mainF(): Unit = {
  val sq: Seq[Double] = Seq(1.0, 2.0, 3.0)
  val mt: Seq[Double] = Seq()
  val retSq = Option.mean(sq)
  val retMt = Option.mean(mt)
  println("Mean of sq: " + retSq)
  println("Mean of mt: " + retMt)
}