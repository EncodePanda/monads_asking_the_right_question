package wtf.monads

sealed trait Many[+A] {
  def andThen[B](f: A => Many[B]): Many[B]

  def within[B](f: A => B): Many[B]
}

object Many {
  def apply[A](elements: A*): Many[A] = {
    if(elements.length == 1) Const(elements(0), Empty)
    else Const(elements(0), apply(elements.drop(1) : _*))
  }
}

case class Const[+A](head: A, tail: Many[A]) extends Many[A] {
  def andThen[B](f: A => Many[B]): Many[B] = concat( f(head), tail.andThen(f) )

  private def concat[A](first: Many[A], second: Many[A]): Many[A] = first match {
    case Empty => second
    case Const(h,t) if t == Empty => Const(h, second)
    case Const(h,t) => Const(h, concat(t, second))
  }

  def within[B](f: A => B): Many[B] = Const(f(head), tail.within(f))
}

case object Empty extends Many[Nothing] {
  def andThen[B](f: Nothing => Many[B]): Many[B] = this

  def within[B](f: Nothing => B): Many[B] = this
}
