

object PolyMorphic {

    // HOF
    def findFirst[A](ds: Array[A], p: A => Boolean): Int = {
        @annotation.tailrec
        def loop(n: Int): Int = {
            if (n >= ds.length) then -1
            else if (p(ds(n))) n
            else loop(n + 1)

        }
        loop(0)
    }

    // HOF
    def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
        @annotation.tailrec
        def loop(n: Int): Boolean = {
            if (n >= as.length) then true
            else if (gt(as(n-1), as(n))) then false
            else loop(n + 1)
        }
        loop(1)
    }

    def main(args: Array[String]): Unit = {
        val nums = Array(1, 30, 2, 60, 3, 90)
        val sorted = Array(1, 2, 5, 9, 12)
        println(findFirst(nums, x => x == 9))
        println(isSorted(sorted, (a,b) => a > b))
    }
}