/* I have to implement a functor type class because... reasons
I think it's because I need ListF to be a functor to get the F Algebra
* */
trait Functor[F[_]] {
  def map[A, B](f: A => B): F[B]
}

object FunctorInstances {
//  implicit val listFFunctor = {
//    new Functor[ListF] {
//      override def map[A, B](f: A => B): ListF[B, List[B]] = {
//        case NilF => NilF
//        case ConsF(head, tail) => ConsF(head, f(tail)) // Think about this. Why do we call f on the tail.
//      }
//    }
//  }
}