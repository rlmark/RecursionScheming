package int_list

sealed trait IntListF[A]
case class IntNilF[A]() extends IntListF[A]
case class IntConsF[A](h: Int, intTailF: A) extends IntListF[A]

object IntListF {
  // IntList and IntListF are ISOMORPHISMS
  def in: IntListF[IntList] => IntList = {
    case _:IntNilF[IntList] => INil
    case IntConsF(head, tail: ICons) => ICons(head, tail)
  }

  def out: IntList => IntListF[IntList] = {
    case INil => IntNilF()
    case ICons(head, tail) => IntConsF[IntList](head, tail)
  }
  /* Because IntList and IntListF are isomorphisms we have the ability to go from our Higher Kinded type F[R] to R
  F[R] => R (in)
  and back!
  R => F[R] (out)
  * */

  // weirdly, IntListF can also be a non-list, but that is not as useful to us
  val notAList = IntConsF(4, "HEY I AM NOT A TAIL")

  // NOOooOOooo type sadness
  val testIntListF: IntListF[IntConsF[IntConsF[IntNilF[Nothing]]]] = IntConsF(1, IntConsF(2, IntConsF(3, IntNilF())))
  // which is why we need fixed points
  // type Fix[F[_]] = F[Fix[_]] but... this won't compile
}
