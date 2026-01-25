package com.resy

import org.mockito.Mockito.when
import org.mockito.Mockito.mock
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.Source

class ResyClientFindSummarySpec extends AnyFlatSpec with Matchers {

  behavior of "ResyClient.getFindSummary"

  it should "parse venue name and slot summary when present" in {
    // scalafix:off
    val resyApi: ResyApi = mock(classOf[ResyApi])
    // scalafix:on
    val resyClient = new ResyClient(resyApi)

    when(resyApi.getReservationsWithStatus("2099-01-30", 2, 12345))
      .thenReturn(Future((200, Source.fromResource("getReservationsWithVenue.json").mkString)))

    val summary = resyClient.getFindSummary("2099-01-30", 2, 12345).get
    summary.venueName shouldEqual Some("Testaurant")
    summary.slotCount shouldEqual 2
    summary.times shouldEqual Seq("16:00:00", "17:00:00")
  }
}
