package com.resy

import org.apache.logging.log4j.scala.Logging
import org.joda.time.DateTime
import play.api.libs.json.{JsArray, JsValue, Json}

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

class ResyClient(
  resyApi: ResyApi,
  events: ResyEventSink = ResyEventSink.noop,
  runInfo: Option[ResyRunInfo] = None
) extends Logging {

  private type ReservationMap = Map[String, TableTypeMap]
  private type TableTypeMap   = Map[String, String]

  import ResyClientErrorMessages._
  import ResyClient._

  /** Tries to find a reservation based on the priority list of requested reservations times. Due to
    * race condition of when the bot runs and when the times become available, retry may be
    * required.
    * @param date
    *   Date of the reservation in YYYY-MM-DD format
    * @param partySize
    *   Size of the party reservation
    * @param venueId
    *   Unique identifier of the restaurant where you want to make the reservation
    * @param resTimeTypes
    *   Priority list of reservation times and table types. Time is in military time HH:MM:SS
    *   format.
    * @param millisToRetry
    *   Optional parameter for how long to try to find a reservations in milliseconds
    * @return
    *   configId which is the unique identifier for the reservation
    */
  def findReservations(
    date: String,
    partySize: Int,
    venueId: Int,
    resTimeTypes: Seq[ReservationTimeType],
    millisToRetry: Long = (10 seconds).toMillis
  ): Try[String] =
    retryFindReservations(
      date,
      partySize,
      venueId,
      resTimeTypes,
      millisToRetry,
      DateTime.now.getMillis,
      attempt = 1
    )

  /** Get details of the reservation
    * @param configId
    *   Unique identifier for the reservation
    * @param date
    *   Date of the reservation in YYYY-MM-DD format
    * @param partySize
    *   Size of the party reservation
    * @return
    *   The paymentMethodId and the bookingToken of the reservation
    */
  def getReservationDetails(configId: String, date: String, partySize: Int): Try[BookingDetails] = {
    val bookingDetailsResp = Try {
      val response = Await.result(
        awaitable = resyApi.getReservationDetails(configId, date, partySize),
        atMost    = 10 seconds
      )

      logger.debug(s"URL Response: $response")

      val resDetails = Json.parse(response)

      // Searching this JSON structure...
      // {"user": {"payment_methods": [{"id": 42, ...}]}}
      val paymentMethodId =
        (resDetails \ "user" \ "payment_methods" \ 0 \ "id").get.toString

      logger.info(s"Payment Method Id: $paymentMethodId")

      // Searching this JSON structure...
      // {"book_token": {"value": "BOOK_TOKEN", ...}}
      val bookToken =
        (resDetails \ "book_token" \ "value").get.toString
          .drop(1)
          .dropRight(1)

      logger.info(s"Book Token: $bookToken")

      BookingDetails(paymentMethodId.toInt, bookToken)
    }

    bookingDetailsResp match {
      case Success(bookingDetails) =>
        Success(bookingDetails)
      case _ =>
        logger.info("Missed the shot!")
        logger.info("""┻━┻ ︵ \(°□°)/ ︵ ┻━┻""")
        logger.info(unknownErrorMsg)
        Failure(new RuntimeException(unknownErrorMsg))
    }
  }

  /** Book the reservation
    * @param paymentMethodId
    *   Unique identifier of the payment id in case of a late cancellation fee
    * @param bookToken
    *   Unique identifier of the reservation in question
    * @return
    *   Unique identifier of the confirmed booking
    */
  def bookReservation(paymentMethodId: Int, bookToken: String): Try[String] = {
    val resyTokenResp = Try {
      val response = Await.result(
        awaitable = resyApi.postReservation(paymentMethodId, bookToken),
        atMost    = 10 seconds
      )

      logger.debug(s"URL Response: $response")

      // Searching this JSON structure...
      // {"resy_token": "RESY_TOKEN", ...}
      (Json.parse(response) \ "resy_token").get.toString
        .drop(1)
        .dropRight(1)
    }

    resyTokenResp match {
      case Success(resyToken) =>
        logger.info("Headshot!")
        logger.info("(҂‾ ▵‾)︻デ═一 (× _ ×#")
        logger.info("Successfully sniped reservation")
        logger.info(s"Resy token is $resyToken")
        Success(resyToken)
      case _ =>
        logger.info("Missed the shot!")
        logger.info("""┻━┻ ︵ \(°□°)/ ︵ ┻━┻""")
        logger.info(resNoLongerAvailMsg)
        Failure(new RuntimeException(resNoLongerAvailMsg))
    }
  }

  /** Calls the "find" endpoint and parses a small summary (for startup sanity checks, etc). */
  def getFindSummary(date: String, partySize: Int, venueId: Int): Try[FindSummary] = Try {
    val (status, body) = Await.result(
      awaitable = resyApi.getReservationsWithStatus(date, partySize, venueId),
      atMost    = 10.seconds
    )

    // Resy returns JSON error bodies for auth failures; preserve a snippet for logs.
    if (status / 100 != 2) {
      val snippet = body.take(400).replaceAll("\\s+", " ")
      throw new RuntimeException(s"find endpoint HTTP $status body=${snippet}")
    }

    val json     = Json.parse(body)
    val venueObj = (json \ "results" \ "venues" \ 0)

    val venueName =
      (venueObj \ "venue" \ "name").asOpt[String]
        .orElse((venueObj \ "venue" \ "display_name").asOpt[String])
        .orElse((venueObj \ "name").asOpt[String])

    val slots =
      (venueObj \ "slots").asOpt[JsArray]
        .map(_.value.toSeq)
        .getOrElse(Seq.empty)

    val times = slots
      .flatMap { slot =>
        (slot \ "date" \ "start").asOpt[String]
          .map(_.dropWhile(_ != ' ').drop(1))
      }
      .distinct
      .sorted

    FindSummary(venueName = venueName, slotCount = slots.size, times = times)
  }

  def logFindSummary(date: String, partySize: Int, venueId: Int): Try[FindSummary] = {
    getFindSummary(date, partySize, venueId).map { summary =>
      val venueStr = summary.venueName.map(n => s"venue=\"$n\" ").getOrElse("")
      if (summary.slotCount == 0) {
        logger.info(s"${venueStr}No slots returned for venueId=$venueId date=$date partySize=$partySize")
      } else {
        val suffix = if (summary.times.size > 10) ",..." else ""
        logger.info(
          s"${venueStr}Found ${summary.slotCount} slot(s) for venueId=$venueId date=$date partySize=$partySize; " +
            s"times=${summary.times.take(10).mkString(",")}$suffix"
        )
      }
      runInfo.foreach(ri => events.findSummary(ri, summary))
      summary
    } recoverWith { case e =>
      logger.info(
        s"Failed to fetch slots for venueId=$venueId date=$date partySize=$partySize (${e.getMessage})"
      )
      Failure(e)
    }
  }

  @tailrec
  private[this] def retryFindReservations(
    date: String,
    partySize: Int,
    venueId: Int,
    resTimeTypes: Seq[ReservationTimeType],
    millisToRetry: Long,
    dateTimeStart: Long,
    attempt: Int
  ): Try[String] = {
    val attemptResult: Try[(ReservationMap, ResyFindAttemptInfo)] = Try {
      val response = Await.result(
        awaitable = resyApi.getReservations(date, partySize, venueId),
        atMost    = 10 seconds
      )

      logger.debug(s"URL Response: $response")

      val json     = Json.parse(response)
      val venueObj = (json \ "results" \ "venues" \ 0)

      val venueName =
        (venueObj \ "venue" \ "name").asOpt[String]
          .orElse((venueObj \ "venue" \ "display_name").asOpt[String])
          .orElse((venueObj \ "name").asOpt[String])

      val slots =
        (venueObj \ "slots").asOpt[JsArray]
          .map(_.value.toSeq)
          .getOrElse(Seq.empty)

      val reservationMap = buildReservationMap(slots)
      val timeToTableTypes =
        reservationMap.map { case (time, tableTypes) =>
          time -> tableTypes.keys.toSeq.sorted
        }

      val info =
        ResyFindAttemptInfo(
          attempt          = attempt,
          venueName        = venueName,
          slotCount        = slots.size,
          times            = reservationMap.keys.toSeq.sorted,
          timeToTableTypes = timeToTableTypes
        )

      (reservationMap, info)
    }

    attemptResult match {
      case Success((reservationMap, info)) =>
        runInfo.foreach(ri => events.findAttempt(ri, info))
        if (reservationMap.nonEmpty)
          findReservationTime(reservationMap, resTimeTypes) match {
            case s @ Success(_) => s
            case Failure(_) if millisToRetry > DateTime.now.getMillis - dateTimeStart =>
              // Treat "slots exist but none match requested time/type" as retriable within the window.
              retryFindReservations(date, partySize, venueId, resTimeTypes, millisToRetry, dateTimeStart, attempt + 1)
            case Failure(e) => Failure(e)
          }
        else if (millisToRetry > DateTime.now.getMillis - dateTimeStart)
          retryFindReservations(date, partySize, venueId, resTimeTypes, millisToRetry, dateTimeStart, attempt + 1)
        else {
          logger.info("Missed the shot!")
          logger.info("""┻━┻ ︵ \(°□°)/ ︵ ┻━┻""")
          logger.info(noAvailableResMsg)
          Failure(new RuntimeException(noAvailableResMsg))
        }
      case Failure(e) if millisToRetry > DateTime.now.getMillis - dateTimeStart =>
        runInfo.foreach { ri =>
          events.findAttempt(
            ri,
            ResyFindAttemptInfo(
              attempt          = attempt,
              venueName        = None,
              slotCount        = 0,
              times            = Nil,
              timeToTableTypes = Map.empty,
              error            = Some(e.getMessage)
            )
          )
        }
        retryFindReservations(date, partySize, venueId, resTimeTypes, millisToRetry, dateTimeStart, attempt + 1)
      case Failure(e) =>
        runInfo.foreach { ri =>
          events.findAttempt(
            ri,
            ResyFindAttemptInfo(
              attempt          = attempt,
              venueName        = None,
              slotCount        = 0,
              times            = Nil,
              timeToTableTypes = Map.empty,
              error            = Some(e.getMessage)
            )
          )
        }
        logger.info("Missed the shot!")
        logger.info("""┻━┻ ︵ \(°□°)/ ︵ ┻━┻""")
        logger.info(noAvailableResMsg)
        Failure(new RuntimeException(noAvailableResMsg))
    }
  }

  @tailrec
  private[this] def findReservationTime(
    reservationMap: ReservationMap,
    resTimeTypes: Seq[ReservationTimeType]
  ): Try[String] = {
    val results = reservationMap.get(resTimeTypes.head.reservationTime).flatMap { tableTypes =>
      resTimeTypes.head.tableType match {
        case Some(tableType) if tableType.nonEmpty => tableTypes.get(tableType.toLowerCase)
        case _                                     => Some(tableTypes.head._2)
      }
    }

    results match {
      case Some(configId) =>
        logger.info(s"Config Id: $configId")
        Success(configId)
      case None if resTimeTypes.tail.nonEmpty =>
        findReservationTime(reservationMap, resTimeTypes.tail)
      case _ =>
        logger.info("Missed the shot!")
        logger.info("""┻━┻ ︵ \(°□°)/ ︵ ┻━┻""")
        logger.info(cantFindResMsg)
        Failure(new RuntimeException(cantFindResMsg))
    }
  }

  private[this] def buildReservationMap(reservationTimes: Seq[JsValue]): ReservationMap = {
    // Build map from these JSON objects...
    // {"config": {"type":"TABLE_TYPE", "token": "CONFIG_ID"},
    // "date": {"start": "2099-01-30 17:00:00"}}
    reservationTimes
      .foldLeft(Map.empty[String, TableTypeMap]) { case (reservationMap, reservation) =>
        val time =
          (reservation \ "date" \ "start").get.toString.dropWhile(_ != ' ').drop(1).dropRight(1)
        val config    = reservation \ "config"
        val tableType = (config \ "type").get.toString.toLowerCase.drop(1).dropRight(1)
        val configId  = (config \ "token").get.toString.drop(1).dropRight(1)

        if (!reservationMap.contains(time))
          reservationMap.updated(time, Map(tableType -> configId))
        else
          reservationMap.updated(time, reservationMap(time).updated(tableType, configId))
      }
  }
}

object ResyClient {
  final case class FindSummary(venueName: Option[String], slotCount: Int, times: Seq[String])
}

object ResyClientErrorMessages {
  val noAvailableResMsg   = "Could not find any available reservations"
  val cantFindResMsg      = "Could not find a reservation for the given time(s)"
  val unknownErrorMsg     = "Unknown error occurred"
  val resNoLongerAvailMsg = "Reservation no longer available"
}

final case class BookingDetails(paymentMethodId: Int, bookingToken: String)
