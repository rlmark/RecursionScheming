package int_list

/* I have to implement a functor type class because... reasons
I think it's because I need ListF to be a functor to get the F Algebra
* */
trait Functor[F[_]] {
  def map[A, B](f: A => B): F[A] => F[B] // Notice that map here returns a FUNCTION from F[A] to F[B]
}

object FunctorInstances {
  implicit val intListFunctor: Functor[IntListF] = { new Functor[Fix[IntListF]] {
      override def map[A, B](f: A => B): Fix[IntListF] => Fix[IntListF] = {
          case Fix(_:IntNilF[A]) => Fix[IntListF](IntNilF())
          case Fix(IntConsF(i, tailF)) => Fix[IntListF](IntConsF(i, Fix(f(tailF))))
      }
    }
  }
}

object Functor {
  def mapF[A,B,F[_]](f: A => B)(implicit functorInstance: Functor[F]) = {
    functorInstance.map(f)
  }
}

object FunctorInstanceRun extends App {
  import Functor._
  import FunctorInstances._
  val testListF: Fix[IntListF] = Fix[IntListF](IntConsF(1, Fix(IntConsF(2, Fix(IntNilF())))))
  mapF((i: Int) => i + 1)(intListFunctor)(testListF.f)
}