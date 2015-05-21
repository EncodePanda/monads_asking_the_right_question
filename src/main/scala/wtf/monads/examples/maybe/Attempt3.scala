package wtf.monads.examples.maybe

import wtf.monads.Maybe

object Attempt3 {

  case class Geek(name: String, partner: Maybe[Geek] = wtf.monads.None, workPlace: Maybe[WorkPlace])

  case class WorkPlace(name: String, street: Maybe[String])


  def partnerLookup(geek: Geek): String =
    geek.partner.andThen(g => g.workPlace).andThen(wp => wp.street) match {
      case wtf.monads.Some(street) => street
      case wtf.monads.None => "not found"
    }


  def main(args: Array[String]) {
    val cheeseCakeFactory = WorkPlace("Cheese Cake Factory", wtf.monads.Some("Cakeer Street 1"))
    val university = WorkPlace("University", wtf.monads.Some("Academic St. 10"))
    var penny = Geek("Penny", workPlace = wtf.monads.Some(cheeseCakeFactory))
    var leonard = Geek("Leonard", workPlace = wtf.monads.Some(university))

    penny = penny.copy(partner = wtf.monads.Some(leonard))
    leonard = leonard.copy(partner = wtf.monads.Some(penny))

    val rajesh = Geek("Rajesh", workPlace = wtf.monads.Some(university))

    println(partnerLookup(leonard))
    println(partnerLookup(penny))
    println(partnerLookup(rajesh))

  }

}
