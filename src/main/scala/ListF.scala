/* Where ListF is a higher kinded type
which contains A (the elements in the list)
and L, a type parameter representing the list
*/
sealed trait ListF[+A, +L]
case class NilF() extends ListF[Nothing, Nothing]
case class ConsF[A, L](head: A, tail: L) extends ListF[A,L]

object ListF {
  def in[A]: ListF[A, List[A]] => List[A] = {
    case _:NilF => Nil
    case ConsF(head, tail) => Cons[A](head, tail)
  }

  def out[A]: List[A] => ListF[A, List[A]] = {
    case Nil => NilF()
    case Cons(head, tail) => ConsF(head, tail)
  }
}
