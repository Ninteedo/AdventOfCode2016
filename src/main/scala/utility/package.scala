import java.util.regex.Pattern
import scala.annotation.tailrec

package object utility {
  extension [T](it: IterableOnce[T]) {
    def distinctCount: Int = it.iterator.distinct.size

    def slidingPairs: Iterator[(T, T)] = {
      val iter = it.iterator
      if (!iter.hasNext) Iterator.empty
      else {
        var prev = iter.next()
        iter.map { next =>
          val pair = (prev, next)
          prev = next
          pair
        }
      }
    }
  }

  extension [T](xs: IndexedSeq[T]) {
    def combinationPairs: Iterator[(T, T)] = for {
      i <- xs.indices.iterator
      j <- (i + 1 until xs.length).iterator
    } yield (xs(i), xs(j))
  }

  extension (b: Boolean) {
    /**
     * Converts a [[Boolean]] to an [[Int]].
     * @return 1 for `true` or 0 for `false`
     */
    def toInt: Int = if b then 1 else 0

    /**
     * @param a  value
     * @tparam A type of [[a]]
     * @return [[Some(a)]] if `true`, otherwise [[None]]
     */
    def thenOption[A](a: => A): Option[A] = if b then Some(a) else None
  }

  extension (b: Byte) {
    def toHexString: String = "%02X".formatted(b)
  }

  extension (n: Int) {
    def inRangeInclusive(min: Int, max: Int): Boolean = n >= min && n <= max

    def positiveModulus(b: Int): Int = {
      val modulus = n % b
      if (modulus < 0) modulus + b else modulus
    }

//    def isNonNegative: Boolean = n >= 0
//
//    def isPositive: Boolean = n > 0
//
//    def isEven: Boolean = n % 2 == 0
//
//    def isOdd: Boolean = n % 2 == 1

//    def half: Int = n / 2

//    def digitCount: Int = n.toString.length - (if (n.isNonNegative) 0 else 1)
  }

  extension (n: Long) {
    def inRangeInclusive(min: Long, max: Long): Boolean = n >= min && n <= max

    def inRangeExclusive(min: Long, max: Long): Boolean = n >= min && n < max

    def isNonNegative: Boolean = n >= 0

    def isPositive: Boolean = n > 0

    def isEven: Boolean = n % 2 == 0

    def isOdd: Boolean = n % 2 == 1

    def half: Long = n / 2

    def square: Long = n * n

    @tailrec
    def digitCount: Int = {
      if n == 0 then 1
      else if n >= 0 then Math.log10(n.toDouble).toInt + 1
      else if n == Long.MinValue then 19 else (-n).digitCount
    }

    def factors: LazyList[Long] = {
      if (n < 1) throw new IllegalArgumentException("Can only calculate factors of a positive number")
      else LazyList.from(1L to n).filter(m => n % m == 0)
    }

    def pow(b: Long): Long = {
      @tailrec
      def loop(exp: Long, base: Long, acc: Long): Long = {
        if exp == 0 then acc
        else if (exp & 1L) == 1L then loop(exp >>> 1, base * base, acc * base)
        else loop(exp >>> 1, base * base, acc)
      }

      loop(b, n, 1L)
    }

    def positiveModulus(b: Long): Long = {
      val modulus = n % b
      if (modulus < 0) modulus + b else modulus
    }
  }

  extension (s: String) {
    def splitPair(seperator: String | Pattern): (String, String) = {
      val sep = seperator match {
        case s: String => Pattern.compile(s)
        case p: Pattern => p
      }
      val splits = sep.split(s, 2)
      (splits(0), splits(1))
    }
  }

  def enumParse[T, K](values: Array[T], accessor: T => K)(key: K): T = values.find(v => accessor(v) == key) match {
    case Some(value) => value
    case None => throw new IllegalArgumentException(s"'$key' does not correspond to any value.")
  }

  def printAndReturn[T](t: T): T = {
    println(t)
    t
  }

  /**
   * Validates a given value against a specified condition and throws an exception if the condition is not met.
   *
   * @param value         The value to be validated.
   * @param passCondition A function that determines whether the value satisfies the validation condition.
   * @param onFailure     A function that generates an exception when the value fails the validation condition.
   * @tparam T The type of the value being validated.
   * @return The input value if it satisfies the validation condition.
   * @throws Exception Thrown when the value fails validation.
   */
  def validate[T](value: T, passCondition: T => Boolean, onFailure: T => Exception): T = {
    if (passCondition.apply(value)) {
      value
    } else {
      throw onFailure.apply(value)
    }
  }
}
