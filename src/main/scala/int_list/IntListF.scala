package int_list

import list.ListF

sealed trait IntListF[A]
case class IntNilF[A]() extends IntListF[A]
case class IntConsF[A](h: Int, invariantTailF: IntListF[A]) extends IntListF[A]

object IntListF {

  def in: IntListF[IntList] => IntList = {
    case _:IntNilF[IntList] => INil
    case IntConsF(head, tail: ICons) => ICons(head, tail)
  }

  def out: IntList => IntListF[IntList] = {
    case INil => IntNilF()
    case ICons(head, tail) => IntConsF[IntList](head, tail)
  }
  val testIntListF: IntListF[IntList] = IntConsF[IntList](1, IntConsF(2, IntConsF(3, IntNilF())))

}

