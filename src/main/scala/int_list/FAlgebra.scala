package int_list

object FAlgebra {
  // Functors give us the notion of an F Algebra
  type FAlgebra[F[_], A] = F[A] => A

  // our previous code for In/Out between ListF and List is an example of an F Algebra
  def in: FAlgebra[IntListF, IntList] = {
    case _:IntNilF[_] => INil
    case IntConsF(a, t) => ICons(a, t)
  }

  // Is this also an f algebra?
  type PhantomIntList[A] = IntList // Creative, but maybe this shouldn't work...
  def out: FAlgebra[PhantomIntList, IntListF[_]] = {
    case INil => IntNilF()
    case ICons(h, t) => IntConsF(h, t)
  }

  def multiplyFAlgebra(): FAlgebra[IntListF, Int] = {
    case IntNilF() => 1
    case IntConsF(h: Int, t: Int) => h * t // Weird that H and T are both Ints, how does that work w/out recursion...
  }

  def addFAlgebra(): FAlgebra[IntListF, Int] = {
    case IntNilF() => 0
    case IntConsF(h: Int, t: Int) => h + t
  }

  def addOneToEachAlgebra(): FAlgebra[IntListF, IntListF[_]] = {
    case IntNilF() => IntNilF()
    case IntConsF(h: Int, t: IntListF[_]) => IntConsF(h + 1 , t)
  }

}
