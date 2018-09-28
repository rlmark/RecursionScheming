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
}

object RunCata extends App {
  import FAlgebra._
  import Cata._
  import Fix._
  import FunctorInstances._

  val testListF: IntListF[IntConsF[IntNilF[Nothing]]] = IntConsF(1, IntConsF(2, IntNilF()))
  val testListFix: Fix[IntListF] = Fix[IntListF](IntConsF(1, Fix(IntConsF(2, Fix(IntNilF())))))

  def multiply: Fix[IntList] => Int = cata(Fix(IntListF.out), multiplyFAlgebra())
  //multiply(testListF)
  multiply(Fix.out(testListFix))

}
