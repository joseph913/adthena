import scala.collection.mutable.ListBuffer

object PriceBasket {

  object Money {
    def apply(pence: Long = 0): Money = Money(pence / 100, pence % 100)
  }

  case class Money(
    pounds: Long,
    pence: Long
  ) {
    val allPence: Long = pounds * 100 + pence

    def +(sum: Money): Money = Money(allPence + sum.allPence)

    def -(sum: Money): Money = Money(allPence - sum.allPence)

    override def toString: String = "%d.%02d".format(pounds, pence)
  }

  case class SpecialOffer(
    buyNum: Long,
    getItem: String,
    getNum: Long,
    discountPct: Long
  )

  private val PriceList = Map(
    "Soup" -> Money(0, 65),
    "Bread" -> Money(0, 80),
    "Milk" -> Money(1, 30),
    "Apples" -> Money(1, 0),
  )

  private val SpecialOffers = Map(
    "Apples" -> SpecialOffer(1, "Apples", 1, 10),
    "Soup" -> SpecialOffer(2, "Bread", 1, 50),
  )

  private def getReceipt(items: List[String]): String = {
    val itemSet: Set[String] = items.toSet
    var subtotal: Money = Money()
    var discountTotal: Money = Money()
    val appliedSpecials: ListBuffer[String] = new ListBuffer[String]()

    items.groupBy(identity).foreach {
      case (item, occurrences) =>
        if (SpecialOffers.contains(item) &&
            occurrences.size >= SpecialOffers(item).buyNum &&
            itemSet.contains(SpecialOffers(item).getItem)) {
          val specialAppliedTimes: Long = occurrences.size / SpecialOffers(item).buyNum
          val subtractMoney: Long = (PriceList
            .getOrElse(SpecialOffers(item).getItem, Money())
            .allPence / 100.0 * SpecialOffers(item).discountPct).toLong
          val subtractPerPosition: Money = Money(subtractMoney * specialAppliedTimes)
          appliedSpecials
            .append(s"${SpecialOffers(item).getItem} ${SpecialOffers(item).discountPct}% off: $subtractPerPosition")
          discountTotal += subtractPerPosition
        }

        subtotal += Money(PriceList.getOrElse(item, Money()).allPence * occurrences.size)
    }

    appliedSpecials.prepend(s"Subtotal: £$subtotal")
    appliedSpecials.append(s"Total price: £${subtotal - discountTotal}")
    appliedSpecials.mkString("\n")
  }

  def main(args: Array[String]): Unit = {
//    val qwe = List("Apples", "Milk", "Bread", "Apples", "Milk", "Bread", "Soup", "Soup", "Soup", "Soup")

    println(getReceipt(args.toList))
  }

}
