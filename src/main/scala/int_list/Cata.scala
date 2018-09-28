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

  // if you thinka bout it, your F algebra is kind of like your
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

  def multiply1 = cata(IntListF.out, multiplyFAlgebra())_

  def multiply2: Fix[IntListF] => Int = cata2(Fix.out, multiplyFAlgebra())
  def addOneToEach = cata2(Fix.out, addOneToEachAlgebra())
  //multiply(testListF)
  println(multiply2(testListFFix))
  println(multiply2(fromList(1 :: 2 :: 3 :: 4 :: 5 :: Nil)))

  // Is a Catamorphism as expressive as a fold??? Can I use it to make a new list as a kind of weird map?
  println((1 to 10).toList.foldLeft(List[Int]())((acc, next) => next + 1 :: acc ))
  println(addOneToEach(testListFFix))
}
