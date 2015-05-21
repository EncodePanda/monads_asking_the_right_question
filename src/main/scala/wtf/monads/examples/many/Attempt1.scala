package wtf.monads.examples.many

import wtf.monads.{Empty, Many}


object Attempt1 {


    object Magic {

      case class RichMany[A](m: Many[A]) {
        def flatMap[B](f: A => Many[B]): Many[B] = m.andThen(f)
        def map[B](f: A => B): Many[B] = m.within(f)
      }

      implicit def enrich[A](m: Many[A]) = RichMany(m)

    }


  def main(args: Array[String]) {

    case class Pirate(name: String, ships: Many[Ship])
    case class Ship(name: String, hold: Hold)
    case class Hold(barrel: Many[Barrel])
    case class Barrel(amount: Int)


    val blackHold = Hold(Empty)
    val whiteHold = Hold(Many(Barrel(20), Barrel(10)))

    val blackPearl = Ship("Black Pearl", blackHold)
    val whitePearl = Ship("White Pearl", whiteHold)
    val jack = Pirate("Captain Jack Sparrow", Many(blackPearl, whitePearl))


    jack.ships.andThen(ship => ship.hold.barrel)
      .within(barrel => barrel.amount)

    import Magic._

    val amounts = for {
      ship <- jack.ships
      barrel <- ship.hold.barrel
    } yield (barrel.amount)

    println(amounts)

  }

}
