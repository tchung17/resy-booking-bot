package com.resy

import com.resy.ResyApi.{sendGetRequest, sendPostRequest}
import org.apache.logging.log4j.scala.Logging
import play.api.libs.json.Json

import java.net.URLEncoder
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

// Resy API docs can be found here http://subzerocbd.info/
class ResyApi(resyToken: ResyKeys) {

  /** Find available reservations
    * @param date
    *   Date of reservation to search in YYYY-MM-DD format
    * @param partySize
    *   Size of party
    * @param venueId
    *   Unique identifier of the restaurant
    * @return
    *   JSON object of available reservation times and seating types for that day
    */
  def getReservations(date: String, partySize: Int, venueId: Int): Future[String] = {
    ResyApi.sendFindRequest(resyToken, date, partySize, venueId)
  }

  /** Same as getReservations, but preserves HTTP status for better diagnostics. */
  def getReservationsWithStatus(date: String, partySize: Int, venueId: Int): Future[(Int, String)] = {
    ResyApi
      .sendFindRequestResponse(resyToken, date, partySize, venueId)
      .map { case (status, body, _) => (status, body) }
  }

  /** Get details of the reservation
    * @param configId
    *   Unique identifier for the reservation
    * @param date
    *   Date of reservation to get details for in YYYY-MM-DD format
    * @param partySize
    *   Size of party
    * @return
    *   JSON object with the details about the reservation
    */
  def getReservationDetails(configId: String, date: String, partySize: Int): Future[String] = {
    val findResQueryParams =
      Map(
        "config_id"  -> configId,
        "day"        -> date,
        "party_size" -> partySize.toString
      )

    sendGetRequest(resyToken, "api.resy.com/3/details", findResQueryParams)
  }

  /** Book the reservation
    * @param paymentMethodId
    *   Unique identifier of the payment id in case of a late cancellation fee
    * @param bookToken
    *   Unique identifier of the reservation in question
    * @return
    *   JSON object of the unique identifier of the confirmed booking
    */
  def postReservation(paymentMethodId: Int, bookToken: String): Future[String] = {
    val bookResQueryParams = Map(
      "book_token"            -> bookToken,
      "struct_payment_method" -> s"""{"id":$paymentMethodId}"""
    )

    sendPostRequest(resyToken, "api.resy.com/3/book", bookResQueryParams)
  }
}

object ResyApi extends Logging {
  private lazy val proxy: Option[ResyProxy] = {
    val p = ResyProxy.fromEnv()
    p.foreach(pp => logger.info(s"HTTP proxy enabled: ${pp.redacted}"))
    p
  }
  private lazy val http                     = new CurlHttpClient(proxy)

  def selectedProxy: Option[ResyProxy] = proxy

  def shutdown(): Future[Unit] = Future.successful(())

  private def sendGetRequest(
    resyKeys: ResyKeys,
    baseUrl: String,
    queryParams: Map[String, String]
  ): Future[String] = {
    val url =
      s"https://$baseUrl?${stringifyQueryParams(queryParams)}"

    logger.debug(s"URL Request: $url")

    Future {
      http
        .request(
          method  = "GET",
          url     = url,
          headers = createHeaders(resyKeys),
          body    = None,
          timeout = 4.seconds
        )
        .body
    }
  }

  private[resy] def sendFindRequest(
    resyKeys: ResyKeys,
    date: String,
    partySize: Int,
    venueId: Int
  ): Future[String] =
    sendFindRequestResponse(resyKeys, date, partySize, venueId).map { case (_, body, _) => body }

  /** Mirrors the Resy web app's /4/find behavior (POST+JSON) to reduce WAF variance. */
  private[resy] def sendFindRequestResponse(
    resyKeys: ResyKeys,
    date: String,
    partySize: Int,
    venueId: Int
  ): Future[(Int, String, String)] = {
    val url = "https://api.resy.com/4/find"
    val bodyJson = Json.obj(
      "lat"        -> 0,
      "long"       -> 0,
      "day"        -> date,
      "party_size" -> partySize,
      "venue_id"   -> venueId
    )

    logger.debug(s"URL Request: $url")

    val headers =
      createHeaders(resyKeys) ++ Seq(
        "accept"                -> "application/json, text/plain, */*",
        "content-type"          -> "application/json",
        "origin"                -> "https://resy.com",
        "referer"               -> "https://resy.com/",
        "x-origin"              -> "https://resy.com"
      )

    Future {
      val resp = http.request(
        method  = "POST",
        url     = url,
        headers = headers,
        body    = Some(Json.stringify(bodyJson)),
        timeout = 4.seconds
      )
      (resp.status, resp.body, resp.headers)
    }
  }

  private def sendPostRequest(
    resyKeys: ResyKeys,
    baseUrl: String,
    queryParams: Map[String, String]
  ): Future[String] = {
    val url  = s"https://$baseUrl"
    val post = stringifyQueryParams(queryParams)

    logger.debug(s"URL Request: $url")
    logger.debug(s"Post Params: $post")

    val headers =
      createHeaders(resyKeys) ++ Seq(
        "Content-Type" -> "application/x-www-form-urlencoded",
        "Origin"       -> "https://widgets.resy.com",
        "Referer"      -> "https://widgets.resy.com/"
      )

    Future {
      http
        .request(
          method  = "POST",
          url     = url,
          headers = headers,
          body    = Some(post),
          timeout = 4.seconds
        )
        .body
    }
  }

  private[this] def createHeaders(resyKeys: ResyKeys): Seq[(String, String)] = {
    Seq(
      "Authorization"     -> s"""ResyAPI api_key="${resyKeys.apiKey}"""",
      "x-resy-auth-token" -> resyKeys.authToken,
      "x-resy-universal-auth" -> resyKeys.authToken
    )
  }

  private[this] def stringifyQueryParams(queryParams: Map[String, String]): String = {
    queryParams.foldLeft("") { case (acc, (key, value)) =>
      acc + s"$key=${URLEncoder.encode(value, "UTF-8")}&"
    }
  }
}
