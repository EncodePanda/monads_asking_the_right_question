package wtf.monads.examples.maybe

import wtf.monads.Maybe


object Attempt4 {

  case class Geek(name: String, partner: Maybe[Geek] = wtf.monads.None, workPlace: Maybe[WorkPlace])

  case class WorkPlace(name: String, street: Maybe[String])

  case class Address(street: String, city: String)

  object Magic {

    case class RichMaybe[A](m: Maybe[A]) {
      def flatMap[B](f: A => Maybe[B]): Maybe[B] = m.andThen(f)
      def map[B](f: A => B): Maybe[B] = m.within(f)
    }

    implicit def enrich[A](m: Maybe[A]) = RichMaybe(m)

  }


  def partnerLookup(geek: Geek): Maybe[String] = {

    import wtf.monads.examples.maybe.Attempt4.Magic._

    for {
      p <- geek.partner
      wp <- p.workPlace
      s <- wp.street
    } yield (s)

  }


  def main(args: Array[String]) {
    val cheeseCakeFactory = WorkPlace("Cheese Cake Factory", wtf.monads.Some("Cakeer Street 1"))
    val university = WorkPlace("University", wtf.monads.Some("Academic St. 10"))
    var penny = Geek("Penny", workPlace = wtf.monads.Some(cheeseCakeFactory))
    var leonard = Geek("Leonard", workPlace = wtf.monads.Some(university))

    penny = penny.copy(partner = wtf.monads.Some(leonard))
    leonard = leonard.copy(partner = wtf.monads.Some(penny))

    val rajesh = Geek("Rajesh", workPlace = wtf.monads.Some(university))

    partnerLookup(leonard).within(println)
    partnerLookup(penny).within(println)
    partnerLookup(rajesh).within(println)

  }

}
