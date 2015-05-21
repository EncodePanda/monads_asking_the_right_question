package wtf.monads

sealed trait Maybe[+A] {
  def andThen[B](f: A => Maybe[B]): Maybe[B]
  def within[B](f: A => B): Maybe[B]
}

object Maybe {
  def apply[A](value: A) = Some(value)
}

case class Some[+A](value: A) extends Maybe[A] {
  def andThen[B](f: A => Maybe[B]): Maybe[B] = f(value)
  def within[B](f: A => B): Maybe[B] = Maybe(f(value))
}

case object None extends Maybe[Nothing] {
  def andThen[B](f: Nothing => Maybe[B]): Maybe[B] = this
  def within[B](f: Nothing => B): Maybe[B] = this
}