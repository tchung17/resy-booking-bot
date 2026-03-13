package com.resy

import org.mockito.Mockito
import org.mockito.Mockito.{mock, verify, when}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import scala.io.Source
import scala.language.postfixOps
import scala.util.{Failure, Success}

class ResyClientSpec extends AnyFlatSpec with Matchers {

  trait Fixture {
    // scalafix:off
    val resyApi: ResyApi = mock(classOf[ResyApi])
    val resyClient = new ResyClient(
      resyApi,
      findSettings = ResyClient.FindSettings(maxInflight = 1, delayMinMs = 0L, delayMaxMs = 0L)
    )
    // scalafix:on
  }

  import ResyClientSpec._

  behavior of "ResyClientSpec"
  it should "find an available reservation" in new Fixture {
    when(resyApi.getReservations(resDetails.date, resDetails.partySize, resDetails.venueId))
      .thenReturn(Future(Source.fromResource("getReservations.json").mkString))

    resyClient.findReservations(
      date          = resDetails.date,
      partySize     = resDetails.partySize,
      venueId       = resDetails.venueId,
      resTimeTypes  = resDetails.resTimeTypes,
      millisToRetry = (.1 seconds).toMillis
    ) shouldEqual Success("CONFIG_ID5")
  }

  it should "find an available reservation that is not the highest priority" in new Fixture {
    when(resyApi.getReservations(resDetails.date, resDetails.partySize, resDetails.venueId))
      .thenReturn(Future(Source.fromResource("getReservations.json").mkString))

    resyClient.findReservations(
      date      = resDetails.date,
      partySize = resDetails.partySize,
      venueId   = resDetails.venueId,
      resTimeTypes = Seq(
        ReservationTimeType("12:34:56", "TABLE_TYPE_DOES_NOT_EXIST"),
        ReservationTimeType("16:00:00", "TABLE_TYPE1")
      ),
      millisToRetry = (.1 seconds).toMillis
    ) shouldEqual Success("CONFIG_ID1")
  }

  it should "find an available reservation for a time with different table types" in new Fixture {
    when(resyApi.getReservations(resDetails.date, resDetails.partySize, resDetails.venueId))
      .thenReturn(Future(Source.fromResource("getReservations.json").mkString))

    resyClient.findReservations(
      date      = resDetails.date,
      partySize = resDetails.partySize,
      venueId   = resDetails.venueId,
      resTimeTypes = Seq(
        ReservationTimeType("17:00:00", "TABLE_TYPE3")
      ),
      millisToRetry = (.1 seconds).toMillis
    ) shouldEqual Success("CONFIG_ID3")
  }

  it should "find an available reservation and match on case insensitive table type" in new Fixture {
    when(resyApi.getReservations(resDetails.date, resDetails.partySize, resDetails.venueId))
      .thenReturn(Future(Source.fromResource("getReservations.json").mkString))

    resyClient.findReservations(
      date      = resDetails.date,
      partySize = resDetails.partySize,
      venueId   = resDetails.venueId,
      resTimeTypes = Seq(
        ReservationTimeType("18:00:00", "table_TYPE5")
      ),
      millisToRetry = (.1 seconds).toMillis
    ) shouldEqual Success("CONFIG_ID5")
  }

  it should "find an available reservation with empty string as the table type" in new Fixture {
    when(resyApi.getReservations(resDetails.date, resDetails.partySize, resDetails.venueId))
      .thenReturn(Future(Source.fromResource("getReservations.json").mkString))

    resyClient.findReservations(
      date      = resDetails.date,
      partySize = resDetails.partySize,
      venueId   = resDetails.venueId,
      resTimeTypes = Seq(
        ReservationTimeType("18:00:00", "")
      ),
      millisToRetry = (.1 seconds).toMillis
    ) shouldEqual Success("CONFIG_ID5")
  }

  it should "find an available reservation with no table type preference" in new Fixture {
    when(resyApi.getReservations(resDetails.date, resDetails.partySize, resDetails.venueId))
      .thenReturn(Future(Source.fromResource("getReservations.json").mkString))

    resyClient.findReservations(
      date      = resDetails.date,
      partySize = resDetails.partySize,
      venueId   = resDetails.venueId,
      resTimeTypes = Seq(
        ReservationTimeType("17:00:00")
      ),
      millisToRetry = (.1 seconds).toMillis
    ) shouldEqual Success("CONFIG_ID2")
  }

  it should "find an available reservation with wildcard time selecting closest to first configured time" in new Fixture {
    when(resyApi.getReservations(resDetails.date, resDetails.partySize, resDetails.venueId))
      .thenReturn(Future(Source.fromResource("getReservationsThreeTimesUnique.json").mkString))

    resyClient.findReservations(
      date      = resDetails.date,
      partySize = resDetails.partySize,
      venueId   = resDetails.venueId,
      resTimeTypes = Seq(
        ReservationTimeType("19:00:00"),
        ReservationTimeType("", "")
      ),
      millisToRetry = (.1 seconds).toMillis
    ) shouldEqual Success("CONFIG_ID_C")
  }

  it should "find an available reservation with wildcard time tie-breaking to earlier time" in new Fixture {
    when(resyApi.getReservations(resDetails.date, resDetails.partySize, resDetails.venueId))
      .thenReturn(Future(Source.fromResource("getReservationsFourTimesUnique.json").mkString))

    resyClient.findReservations(
      date      = resDetails.date,
      partySize = resDetails.partySize,
      venueId   = resDetails.venueId,
      resTimeTypes = Seq(
        ReservationTimeType("18:30:00"),
        ReservationTimeType("", "")
      ),
      millisToRetry = (.1 seconds).toMillis
    ) shouldEqual Success("CONFIG_ID_3")
  }

  it should "find an available reservation with wildcard time and no anchor falling back to median" in new Fixture {
    when(resyApi.getReservations(resDetails.date, resDetails.partySize, resDetails.venueId))
      .thenReturn(Future(Source.fromResource("getReservationsThreeTimesUnique.json").mkString))

    resyClient.findReservations(
      date      = resDetails.date,
      partySize = resDetails.partySize,
      venueId   = resDetails.venueId,
      resTimeTypes = Seq(
        ReservationTimeType("", "")
      ),
      millisToRetry = (.1 seconds).toMillis
    ) shouldEqual Success("CONFIG_ID_B")
  }

  it should "find an available reservation with wildcard time and a table type preference" in new Fixture {
    when(resyApi.getReservations(resDetails.date, resDetails.partySize, resDetails.venueId))
      .thenReturn(Future(Source.fromResource("getReservations.json").mkString))

    resyClient.findReservations(
      date      = resDetails.date,
      partySize = resDetails.partySize,
      venueId   = resDetails.venueId,
      resTimeTypes = Seq(
        ReservationTimeType("", "TABLE_TYPE5")
      ),
      millisToRetry = (.1 seconds).toMillis
    ) shouldEqual Success("CONFIG_ID5")
  }

  it should "find an available reservation after a bad response with retrying" in new Fixture {
    when(resyApi.getReservations(resDetails.date, resDetails.partySize, resDetails.venueId))
      .thenReturn(Future(""))
      .thenReturn(Future(Source.fromResource("getReservations.json").mkString))

    resyClient.findReservations(
      date          = resDetails.date,
      partySize     = resDetails.partySize,
      venueId       = resDetails.venueId,
      resTimeTypes  = resDetails.resTimeTypes,
      millisToRetry = (.1 seconds).toMillis
    ) shouldEqual Success("CONFIG_ID5")

    verify(resyApi, Mockito.times(2))
      .getReservations(
        date      = resDetails.date,
        partySize = resDetails.partySize,
        venueId   = resDetails.venueId
      )
  }

  it should "find an available reservation after a response of no reservations with retrying" in new Fixture {
    when(resyApi.getReservations(resDetails.date, resDetails.partySize, resDetails.venueId))
      .thenReturn(Future(Source.fromResource("getReservationsNoneAvailable.json").mkString))
      .thenReturn(Future(Source.fromResource("getReservations.json").mkString))

    resyClient.findReservations(
      date          = resDetails.date,
      partySize     = resDetails.partySize,
      venueId       = resDetails.venueId,
      resTimeTypes  = resDetails.resTimeTypes,
      millisToRetry = (.1 seconds).toMillis
    ) shouldEqual Success("CONFIG_ID5")

    verify(resyApi, Mockito.times(2))
      .getReservations(
        date      = resDetails.date,
        partySize = resDetails.partySize,
        venueId   = resDetails.venueId
      )
  }

  it should "not find an available reservation due to bad response" in new Fixture {
    when(resyApi.getReservations(resDetails.date, resDetails.partySize, resDetails.venueId))
      .thenReturn(Future(""))

    resyClient.findReservations(
      date          = resDetails.date,
      partySize     = resDetails.partySize,
      venueId       = resDetails.venueId,
      resTimeTypes  = resDetails.resTimeTypes,
      millisToRetry = (.1 seconds).toMillis
    ) match {
      case Failure(exception) =>
        exception match {
          case _: RuntimeException =>
            exception.getMessage shouldEqual ResyClientErrorMessages.noAvailableResMsg
          case _ => fail("RuntimeException not found")
        }
      case _ => fail("Failure not found")
    }
  }

  it should "not find an available reservation where time matches and table type doesn't" in new Fixture {
    when(resyApi.getReservations(resDetails.date, resDetails.partySize, resDetails.venueId))
      .thenReturn(Future(Source.fromResource("getReservations.json").mkString))

    resyClient.findReservations(
      date         = resDetails.date,
      partySize    = resDetails.partySize,
      venueId      = resDetails.venueId,
      resTimeTypes = Seq(ReservationTimeType("18:00:00", "TABLE_TYPE_DOES_NOT_EXIST")),
      millisToRetry = 0L
    ) match {
      case Failure(exception) =>
        exception match {
          case _: RuntimeException =>
            exception.getMessage shouldEqual ResyClientErrorMessages.cantFindResMsg
          case _ => fail("RuntimeException not found")
        }
      case _ => fail("Failure not found")
    }
  }

  it should "not find an available reservation where time doesn't match and table type does" in new Fixture {
    when(resyApi.getReservations(resDetails.date, resDetails.partySize, resDetails.venueId))
      .thenReturn(Future(Source.fromResource("getReservations.json").mkString))

    resyClient.findReservations(
      date         = resDetails.date,
      partySize    = resDetails.partySize,
      venueId      = resDetails.venueId,
      resTimeTypes = Seq(ReservationTimeType("12:34:56", "TABLE_TYPE5")),
      millisToRetry = 0L
    ) match {
      case Failure(exception) =>
        exception match {
          case _: RuntimeException =>
            exception.getMessage shouldEqual ResyClientErrorMessages.cantFindResMsg
          case _ => fail("RuntimeException not found")
        }
      case _ => fail("Failure not found")
    }
  }

  it should "not find an available reservation where neither time nor table type matches" in new Fixture {
    when(resyApi.getReservations(resDetails.date, resDetails.partySize, resDetails.venueId))
      .thenReturn(Future(Source.fromResource("getReservations.json").mkString))

    resyClient.findReservations(
      date         = resDetails.date,
      partySize    = resDetails.partySize,
      venueId      = resDetails.venueId,
      resTimeTypes = Seq(ReservationTimeType("12:34:56", "TABLE_TYPE_DOES_NOT_EXIST")),
      millisToRetry = 0L
    ) match {
      case Failure(exception) =>
        exception match {
          case _: RuntimeException =>
            exception.getMessage shouldEqual ResyClientErrorMessages.cantFindResMsg
          case _ => fail("RuntimeException not found")
        }
      case _ => fail("Failure not found")
    }
  }

  it should "retry when slots exist but target time/type is not present yet" in new Fixture {
    when(resyApi.getReservations(resDetails.date, resDetails.partySize, resDetails.venueId))
      .thenReturn(Future(Source.fromResource("getReservationsNoTargetTime.json").mkString))
      .thenReturn(Future(Source.fromResource("getReservations.json").mkString))

    resyClient.findReservations(
      date          = resDetails.date,
      partySize     = resDetails.partySize,
      venueId       = resDetails.venueId,
      resTimeTypes  = resDetails.resTimeTypes,
      millisToRetry = (1 second).toMillis
    ) shouldEqual Success("CONFIG_ID5")

    verify(resyApi, Mockito.times(2))
      .getReservations(
        date      = resDetails.date,
        partySize = resDetails.partySize,
        venueId   = resDetails.venueId
      )
  }

  it should "find an available reservation with parallel /find requests" in {
    val resyApi: ResyApi = mock(classOf[ResyApi]) // scalafix:off
    val resyClient = new ResyClient(
      resyApi,
      findSettings = ResyClient.FindSettings(maxInflight = 2, delayMinMs = 0L, delayMaxMs = 0L)
    ) // scalafix:on

    val stalled = Promise[String]()
    when(resyApi.getReservations(resDetails.date, resDetails.partySize, resDetails.venueId))
      .thenReturn(stalled.future)
      .thenReturn(Future(Source.fromResource("getReservations.json").mkString))

    val result =
      resyClient.findReservations(
        date          = resDetails.date,
        partySize     = resDetails.partySize,
        venueId       = resDetails.venueId,
        resTimeTypes  = resDetails.resTimeTypes,
        millisToRetry = (1 second).toMillis
      )

    // Unblock any still-running /find attempt (we currently don't cancel in-flight requests).
    stalled.trySuccess(Source.fromResource("getReservationsNoTargetTime.json").mkString)

    result shouldEqual Success("CONFIG_ID5")
    verify(resyApi, Mockito.atLeast(2))
      .getReservations(
        date      = resDetails.date,
        partySize = resDetails.partySize,
        venueId   = resDetails.venueId
      )
  }

  it should "get reservation details" in new Fixture {
    when(resyApi.getReservationDetails(configId, resDetails.date, resDetails.partySize))
      .thenReturn(Future(Source.fromResource("getReservationDetails.json").mkString))

    resyClient.getReservationDetails(
      configId  = configId,
      date      = resDetails.date,
      partySize = resDetails.partySize
    ) shouldEqual Success(BookingDetails(42, "BOOK_TOKEN"))
  }

  it should "not get reservation details due to bad response" in new Fixture {
    when(resyApi.getReservationDetails(configId, resDetails.date, resDetails.partySize))
      .thenReturn(Future(""))

    resyClient.getReservationDetails(
      configId  = configId,
      date      = resDetails.date,
      partySize = resDetails.partySize
    ) match {
      case Failure(exception) =>
        exception match {
          case _: RuntimeException =>
            exception.getMessage shouldEqual ResyClientErrorMessages.unknownErrorMsg
          case _ => fail("RuntimeException not found")
        }
      case _ => fail("Failure not found")
    }
  }

  it should "book reservation" in new Fixture {
    when(resyApi.postReservation(paymentMethodId, bookToken))
      .thenReturn(Future(Source.fromResource("bookReservation.json").mkString))

    resyClient.bookReservation(
      paymentMethodId = paymentMethodId,
      bookToken       = bookToken
    ) shouldEqual Success("RESY_TOKEN")
  }

  it should "not book reservation due to bad response" in new Fixture {
    when(resyApi.postReservation(paymentMethodId, bookToken))
      .thenReturn(Future(""))

    resyClient.bookReservation(
      paymentMethodId = paymentMethodId,
      bookToken       = bookToken
    ) match {
      case Failure(exception) =>
        exception match {
          case _: RuntimeException =>
            exception.getMessage shouldEqual ResyClientErrorMessages.resNoLongerAvailMsg
          case _ => fail("RuntimeException not found")
        }
      case _ => fail("Failure not found")
    }
  }
}

object ResyClientSpec {

  val resDetails: ReservationDetails = ReservationDetails(
    date         = "2099-01-30",
    partySize    = 2,
    venueId      = 1,
    resTimeTypes = Seq(ReservationTimeType("18:00:00", "TABLE_TYPE5"))
  )

  val configId = "CONFIG_ID"

  val paymentMethodId = 42
  val bookToken       = "BOOK_TOKEN"
}
