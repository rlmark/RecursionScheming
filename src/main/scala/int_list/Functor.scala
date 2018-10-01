package int_list

/* I have to implement a functor type class because... reasons
I think it's because I need ListF to be a functor to get the F Algebra
* */
trait Functor[F[_]] {
  def map[A, B](f: A => B): F[A] => F[B] // Notice that map here returns a FUNCTION from F[A] to F[B]
}

object FunctorInstances {
  implicit val intListFunctor: Functor[IntListF] = { new Functor[IntListF] {
      override def map[A, B](f: A => B): IntListF[A] => IntListF[B] = {
          case _:IntNilF[A] => IntNilF()
          case IntConsF(i, tailF) => IntConsF(i, f(tailF))
      }
    }
  }
}

object Functor {
  def mapF[A,B,F[_]](f: A => B)(implicit functorInstance: Functor[F]) = {
    functorInstance.map(f)
  }
  def apply[F[_]](implicit f: Functor[F]): Functor[F] = f
}

object FunctorInstanceRun extends App {
  import Functor._
  import FunctorInstances._
  val testListF: Fix[IntListF] = Fix[IntListF](IntConsF(1, Fix(IntConsF(2, Fix(IntNilF())))))
  val testListNonFix: IntListF[IntConsF[IntNilF[Nothing]]] = IntConsF(1, IntConsF(2, IntNilF()))
  mapF((i: Int) => i + 1)(intListFunctor)//(testListNonFix)
}