package list

import scala.annotation.tailrec

sealed trait List[+A] {
  final def map[B](f: A => B): List[B] = {
    this match {
      case Nil => Nil
      case Cons(head, tail) => Cons(f(head), tail.map(f))
    }
  }

  final def foldRight[B](z: B)(f: (A, B) => B): B = {
    this match {
      case Nil => z
      case Cons(head, tail) => f(head, tail.foldRight(z)(f))
    }
  }

  @tailrec
  final def foldLeft[B](z: B)(f: (B, A) => B): B = {
    this match {
      case Nil => z
      case Cons(head, tail) => tail.foldLeft(f(z, head))(f)
    }
  }
}

case object Nil extends List[Nothing]
case class Cons[A](head: A, tail: List[A]) extends List[A]

