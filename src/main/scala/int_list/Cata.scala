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

  def cataFix[A, F[_]: Functor[F]](out: Fix[F] => F[Fix[F]], alg: F[A] => A)(r: Fix[F]): A = {
    val functorOfF = Functor[F]
    val mapCata: F[Fix[F]] => F[A] = functorOfF.map(cataFix(out, alg))
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
  def multiply2: Fix[IntListF] => Int = cataFix(Fix.out, multiplyFAlgebra())
  //multiply1(testListF) // this doesn't work

  println(multiply2(testListFFix))
  println(multiply2(fromList(1 :: 2 :: 3 :: 4 :: 5 :: Nil)))

  // Is a Catamorphism as expressive as a fold??? Can I use it to make a new list as a kind of weird map?
  println((1 to 10).toList.foldLeft(List[Int]())((acc, next) => acc :+ next + 1 ))
  // Answer: Yes! You can!
  def addOneToEach: Fix[IntListF] => IntListF[_] = cataFix(Fix.out, addOneToEachAlgebra())
  println(addOneToEach(testListFFix))
}
