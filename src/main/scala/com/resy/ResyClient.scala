package com.resy

import org.apache.logging.log4j.scala.Logging
import org.joda.time.DateTime
import play.api.libs.json.{JsArray, JsValue, Json}

import scala.annotation.tailrec
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

class ResyClient(
  resyApi: ResyApi,
  events: ResyEventSink = ResyEventSink.noop,
  runInfo: Option[ResyRunInfo] = None,
  findSettings: ResyClient.FindSettings = ResyClient.FindSettings.fromEnv()
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
    if (findSettings.maxInflight <= 1)
      retryFindReservations(
        date,
        partySize,
        venueId,
        resTimeTypes,
        millisToRetry,
        DateTime.now.getMillis,
        attempt  = 1,
        sawSlots = false
      )
    else
      findReservationsParallel(date, partySize, venueId, resTimeTypes, millisToRetry)

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
    attempt: Int,
    sawSlots: Boolean
  ): Try[String] = {
    val attemptResult: Try[(ReservationMap, ResyFindAttemptInfo)] =
      fetchReservationMapAttempt(date, partySize, venueId, attempt)

    attemptResult match {
      case Success((reservationMap, info)) =>
        runInfo.foreach(ri => events.findAttempt(ri, info))
        val isWithinWindow = millisToRetry > DateTime.now.getMillis - dateTimeStart
        val nextSawSlots   = sawSlots || reservationMap.nonEmpty

        if (reservationMap.nonEmpty) {
          selectConfigId(reservationMap, resTimeTypes) match {
            case Some(configId) =>
              logger.info(s"Config Id: $configId")
              Success(configId)
            case None if isWithinWindow =>
              retryFindReservations(date, partySize, venueId, resTimeTypes, millisToRetry, dateTimeStart, attempt + 1, nextSawSlots)
            case None =>
              miss(cantFindResMsg)
          }
        } else if (isWithinWindow)
          retryFindReservations(date, partySize, venueId, resTimeTypes, millisToRetry, dateTimeStart, attempt + 1, nextSawSlots)
        else
          miss(noAvailableResMsg)

      case Failure(e) =>
        val isWithinWindow = millisToRetry > DateTime.now.getMillis - dateTimeStart
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
        if (isWithinWindow)
          retryFindReservations(date, partySize, venueId, resTimeTypes, millisToRetry, dateTimeStart, attempt + 1, sawSlots)
        else if (sawSlots)
          miss(cantFindResMsg)
        else
          miss(noAvailableResMsg)
    }
  }

  private[this] def findReservationsParallel(
    date: String,
    partySize: Int,
    venueId: Int,
    resTimeTypes: Seq[ReservationTimeType],
    millisToRetry: Long
  ): Try[String] = {
    if (millisToRetry <= 0L) return miss(noAvailableResMsg)

    val startMs  = DateTime.now.getMillis
    val stopAtMs = startMs + millisToRetry

    val promise  = Promise[String]()
    val sawSlots = new java.util.concurrent.atomic.AtomicBoolean(false)
    val attempt  = new java.util.concurrent.atomic.AtomicInteger(0)

    def timeRemainingMs: Long = stopAtMs - DateTime.now.getMillis

    def workerLoop(): Unit = {
      val remaining = timeRemainingMs
      if (promise.isCompleted || remaining <= 0L) return

      val delayMs = findSettings.nextDelayMs()

      ResyClient.findScheduler.schedule(
        new Runnable {
          override def run(): Unit = {
            val remainingInner = timeRemainingMs
            if (promise.isCompleted || remainingInner <= 0L) return

            val attemptNo = attempt.incrementAndGet()
            Future {
              val attemptResult = fetchReservationMapAttempt(date, partySize, venueId, attemptNo)
              attemptResult match {
                case Success((reservationMap, info)) =>
                  runInfo.foreach(ri => events.findAttempt(ri, info))
                  if (reservationMap.nonEmpty) sawSlots.set(true)
                  selectConfigId(reservationMap, resTimeTypes).foreach { configId =>
                    if (promise.trySuccess(configId)) logger.info(s"Config Id: $configId")
                  }
                case Failure(e) =>
                  runInfo.foreach { ri =>
                    events.findAttempt(
                      ri,
                      ResyFindAttemptInfo(
                        attempt          = attemptNo,
                        venueName        = None,
                        slotCount        = 0,
                        times            = Nil,
                        timeToTableTypes = Map.empty,
                        error            = Some(e.getMessage)
                      )
                    )
                  }
              }
            }.andThen { case _ => workerLoop() }
            ()
          }
        },
        delayMs,
        java.util.concurrent.TimeUnit.MILLISECONDS
      )
    }

    (1 to findSettings.maxInflight).foreach(_ => workerLoop())

    try Success(Await.result(promise.future, millisToRetry.millis))
    catch {
      case _: java.util.concurrent.TimeoutException =>
        if (sawSlots.get()) miss(cantFindResMsg) else miss(noAvailableResMsg)
    }
  }

  private[this] def selectConfigId(
    reservationMap: ReservationMap,
    resTimeTypes: Seq[ReservationTimeType]
  ): Option[String] =
    resTimeTypes.iterator.flatMap { pref =>
      reservationMap.get(pref.reservationTime).flatMap { tableTypes =>
        pref.tableType match {
          case Some(tableType) if tableType.nonEmpty => tableTypes.get(tableType.toLowerCase)
          case _                                     => tableTypes.headOption.map(_._2)
        }
      }
    }.take(1).toSeq.headOption

  private[this] def fetchReservationMapAttempt(
    date: String,
    partySize: Int,
    venueId: Int,
    attempt: Int
  ): Try[(ReservationMap, ResyFindAttemptInfo)] = Try {
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

  private[this] def miss(message: String): Failure[String] = {
    logger.info("Missed the shot!")
    logger.info("""┻━┻ ︵ \(°□°)/ ︵ ┻━┻""")
    logger.info(message)
    Failure(new RuntimeException(message))
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

  final case class FindSettings(
    maxInflight: Int,
    delayMinMs: Long,
    delayMaxMs: Long
  ) {
    def nextDelayMs(): Long = {
      val min = math.max(0L, delayMinMs)
      val max = math.max(min, delayMaxMs)
      if (max == min) min
      else java.util.concurrent.ThreadLocalRandom.current().nextLong(min, max + 1)
    }
  }

  object FindSettings {
    def fromEnv(env: Map[String, String] = sys.env): FindSettings = {
      val maxInflight = env.get("RESY_FIND_CONCURRENCY").flatMap(_.toIntOption).getOrElse(1).max(1)
      val delayMinMs  = env.get("RESY_FIND_DELAY_MIN_MS").flatMap(_.toLongOption).getOrElse(250L).max(0L)
      val delayMaxMs  = env.get("RESY_FIND_DELAY_MAX_MS").flatMap(_.toLongOption).getOrElse(500L).max(delayMinMs)
      FindSettings(maxInflight = maxInflight, delayMinMs = delayMinMs, delayMaxMs = delayMaxMs)
    }
  }

  private[resy] lazy val findScheduler: java.util.concurrent.ScheduledExecutorService = {
    val threadFactory = new java.util.concurrent.ThreadFactory {
      override def newThread(r: Runnable): Thread = {
        val t = new Thread(r)
        t.setName("resy-find-scheduler")
        t.setDaemon(true)
        t
      }
    }
    java.util.concurrent.Executors.newSingleThreadScheduledExecutor(threadFactory)
  }
}

object ResyClientErrorMessages {
  val noAvailableResMsg   = "Could not find any available reservations"
  val cantFindResMsg      = "Could not find a reservation for the given time(s)"
  val unknownErrorMsg     = "Unknown error occurred"
  val resNoLongerAvailMsg = "Reservation no longer available"
}

final case class BookingDetails(paymentMethodId: Int, bookingToken: String)
