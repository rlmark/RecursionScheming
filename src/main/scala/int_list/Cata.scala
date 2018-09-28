package int_list

object Cata {
 /* Our goal is to be able to go from our recursive data type, IntList to a single value A, with a generalizable fold.
 * R => F[R] => F[A] => A
 * IntList => IntListF[IntList] => IntListF[A] => A
 * */

  def cata[A, R, F[_]](out: R => F[R], alg: F[A] => A)(r: R)(implicit functor: Functor[F]): A = {
    val fr: F[R] = out(r)
    val mapCata: F[R] => F[A] = functor.map(cata(out, alg))
    alg(mapCata(fr))
  }

  /*def cata[F[_]: Functor, A](structure: Fix[F])(algebra: F[A] => A): A =
  algebra(structure.unfix.map(cata(_)(algebra)))*/

  def cata2[A, F[_]](out: Fix[F] => F[Fix[F]], alg: F[A] => A)(r: Fix[F])(implicit functor: Functor[F]): A = {
    val mapCata: F[Fix[F]] => F[A] = functor.map(cata2(out, alg))
    alg(mapCata(r.unfix))
  }
}

object RunCata extends App {
  import FAlgebra._
  import Cata._
  import FunctorInstances._

  val testListF: IntListF[IntConsF[IntNilF[Nothing]]] = IntConsF(1, IntConsF(2, IntNilF()))
  val testListFFix: Fix[IntListF] = Fix[IntListF](IntConsF(1, Fix(IntConsF(2, Fix(IntNilF())))))

  def fromList(l: List[Int]): Fix[IntListF] = l match {
    case Nil => Fix(IntNilF())
    case x :: xs => Fix[IntListF](IntConsF(x, fromList(xs)))
  }

//  def multiply = cata(IntListF.out, multiplyFAlgebra())

  val intListOut: IntList => IntListF[IntList] = IntListF.out
  val intListin: IntListF[IntList] => IntList = IntListF.in

  def multiply: Fix[IntListF] => Int = cata2(Fix.out, multiplyFAlgebra())
  //multiply(testListF)
  println(multiply(testListFFix))
  println(multiply(fromList(1 :: 2 :: 3 :: 4 :: 5 :: Nil)))

}
