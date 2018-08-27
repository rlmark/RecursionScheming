package int_list

object Cata {
 /* Our goal is to be able to go from our recursive data type, IntList to a single value A, with a generalizable fold.
 * R => F[R] => F[A] => A
 * IntList => IntListF[IntList] => IntListF[A] => A
 * */

  def cata1[A](r: IntList, out: IntList => IntListF[IntList],
              map: IntListF[IntList] => IntListF[A], alg: IntListF[A] => A):A = {
    val fr = out(r)
    val fa = map(fr)
    val a = alg(fa)
    // alg(map(out(r)))
    a
  }

  def cata2[A, R, F[_]](out: R => F[R], alg: F[A] => A)(r: R)(implicit functor: Functor[F]): A = {
    val fr: F[R] = out(r)
    val mapCata: F[R] => F[A] = functor.map(cata2(out, alg))
    alg(mapCata(fr))
  }
}
