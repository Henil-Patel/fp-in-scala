
import scala.{Option as _, Some as _, None as _}

sealed trait Option[+A]
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

def mean(xs: Seq[Double]): Option[Double] = {
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)
}


@main
def mainF(): Unit = {
  val sq: Seq[Double] = Seq(1.0, 2.0, 3.0)
  val mt: Seq[Double] = Seq()
  val retSq = mean(sq)
  val retMt = mean(mt)
  println("Mean of sq: " + retSq)
  println("Mean of mt: " + retMt)
}