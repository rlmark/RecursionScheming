trait InvariantList {
  def invariantMap(f: Int => Int): InvariantList = {
    this match {
      case INil => INil
      case ICons(head, tail) => ICons(f(head), tail.invariantMap(f))
    }
  }
  // Folds are Catamorphisms
  def iFoldLeft[A](z: A)(f: (A, Int) => A): A = {
    this match {
      case INil => z
      case ICons(h, t) => t.iFoldLeft(f(z, h))(f)
    }
  }

  def iFoldRight[A](z: A)(f: (Int, A) => A): A = {
    this match {
      case INil => z
      case ICons(h, t) => f(h, t.iFoldRight(z)(f))
    }
  }
}
case object INil extends InvariantList
case class ICons(head: Int, tail: InvariantList) extends InvariantList

object InvariantListTest extends App {
  def multiply(l: InvariantList): Int = {
    l match {
      case INil => 1
      case ICons(h, t) => h * multiply(t)
    }
  }
  def length(l: InvariantList): Int = {
    l match {
      case INil => 0
      case ICons(_, t) => 1 + length(t)
    }
  }

  val testListI = ICons(1, ICons(2, ICons(3, INil)))
  println(multiply(testListI))
  println(length(testListI))
  println(testListI.iFoldLeft(1)((acc, next) => next * acc))
  println(testListI.iFoldLeft(0)((acc, _) => acc + 1))
  println(testListI.iFoldRight(1)((acc, next) => next * acc))
  println(testListI.iFoldRight(0)((_, acc) => acc + 1))
}
