package list

/* Where ListF is a higher kinded type
which contains A (the elements in the list)
and L is a type parameter representing the list
*/
sealed trait ListF[+A, L]
case object NilF extends ListF[Nothing, Nothing]
case class ConsF[A, L](head: A, tail: L) extends ListF[A,L]

object ListF {

}