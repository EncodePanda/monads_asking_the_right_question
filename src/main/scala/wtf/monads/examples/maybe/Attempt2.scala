package wtf.monads.examples.maybe

object Attempt2 {

  case class Geek(name: String, partner: Geek = null, workPlace: WorkPlace)

  case class WorkPlace(name: String, street: String)


  def partnerLookup(geek: Geek): String = {

    if(geek != null) {
      if(geek.partner != null) {
        if(geek.partner.workPlace != null) {
          if(geek.partner.workPlace.street != null) {
            return geek.partner.workPlace.street
          }
        }
      }
    }

    "not found"
  }


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
