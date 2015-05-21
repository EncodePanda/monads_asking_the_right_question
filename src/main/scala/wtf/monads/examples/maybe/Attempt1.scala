package wtf.monads.examples.maybe

object Attempt1 {

  case class Geek(name: String, partner: Geek = null, workPlace: WorkPlace)

  case class WorkPlace(name: String, street: String)

  def partnerLookup(geek: Geek): String = geek.partner.workPlace.street

  def main(args: Array[String]) {
    val cheeseCakeFactory = WorkPlace("Cheese Cake Factory", "Cake Street 1")
    val university = WorkPlace("University", "Academic St. 10")
    var penny = Geek("Penny", workPlace = cheeseCakeFactory)
    var leonard = Geek("Leonard", workPlace = university)

    penny = penny.copy(partner = leonard)
    leonard = leonard.copy(partner = penny)

    val rajesh = Geek("Rajesh", workPlace = university)

    println(partnerLookup(leonard))
    println(partnerLookup(penny))
    println(partnerLookup(rajesh))
  }
}
