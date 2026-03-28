package com.resy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsArray, Json}
import pureconfig.ConfigSource
import pureconfig.generic.auto._

import java.nio.file.{Files, Paths}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class ProxyFindHealthSpec extends AnyFlatSpec with Matchers {

  behavior of "ProxyFindHealthSpec"

  it should "run a /find call through each configured proxy" in {
    val enabled = sys.env.get("RESY_PROXY_HEALTHCHECK").contains("1")
    assume(
      enabled,
      "Set RESY_PROXY_HEALTHCHECK=1 to run this network/proxy health check (it is skipped by default)."
    )

    val configPath = sys.env.getOrElse("RESY_CONFIG", "/etc/resy-booking-bot/resyConfig.conf")
    assume(Files.exists(Paths.get(configPath)), s"Config file not found: $configPath")

    val cfg         = ConfigSource.file(configPath)
    val resyKeysE   = cfg.at("resyKeys").load[ResyKeys]
    val resDetailsE = cfg.at("resDetails").load[ReservationDetails]

    val resyKeys = resyKeysE match {
      case Right(v) => v
      case Left(failures) =>
        fail(s"Invalid config at `resyKeys` in $configPath: ${failures.prettyPrint()}")
    }
    val resDetails = resDetailsE match {
      case Right(v) => v
      case Left(failures) =>
        fail(s"Invalid config at `resDetails` in $configPath: ${failures.prettyPrint()}")
    }

    val (proxySourceKey, proxySourceValue) = {
      val keys = Seq("RESY_PROXY", "RESY_PROXY_POOL", "RESY_PROXIES")
      keys.iterator
        .flatMap(k => sys.env.get(k).map(v => (k, v)))
        .take(1)
        .toSeq
        .headOption
        .getOrElse(fail("No proxies configured in env. Set RESY_PROXY / RESY_PROXY_POOL / RESY_PROXIES."))
    }

    val proxies = ResyProxy.parseMany(proxySourceValue)
    proxies.nonEmpty shouldBe true

    val timeoutSeconds =
      sys.env
        .get("RESY_PROXY_FIND_TIMEOUT_SECONDS")
        .flatMap(_.toIntOption)
        .map(_.max(1))
        .getOrElse(4)

    val timeout = timeoutSeconds.seconds

    println(
      s"Proxy /find healthcheck: proxies=${proxies.size} source=$proxySourceKey " +
        s"timeoutSeconds=$timeoutSeconds date=${resDetails.date} partySize=${resDetails.partySize} venueId=${resDetails.venueId}"
    )

    val failures = Vector.newBuilder[String]

    proxies.zipWithIndex.foreach { case (proxy, idx) =>
      val client = new CurlHttpClient(Some(proxy))
      val url    = "https://api.resy.com/4/find"

      val bodyJson = Json.obj(
        "lat"        -> 0,
        "long"       -> 0,
        "day"        -> resDetails.date,
        "party_size" -> resDetails.partySize,
        "venue_id"   -> resDetails.venueId
      )

      val headers =
        Seq(
          "Authorization"         -> s"""ResyAPI api_key="${resyKeys.apiKey}"""",
          "x-resy-auth-token"     -> resyKeys.authToken,
          "x-resy-universal-auth" -> resyKeys.authToken,
          "accept"                -> "application/json, text/plain, */*",
          "content-type"          -> "application/json",
          "origin"                -> "https://resy.com",
          "referer"               -> "https://resy.com/",
          "x-origin"              -> "https://resy.com"
        )

      val startNs = System.nanoTime()
      val respE =
        Try(
          client.request(
            method  = "POST",
            url     = url,
            headers = headers,
            body    = Some(Json.stringify(bodyJson)),
            timeout = timeout
          )
        )

      val tookMs = (System.nanoTime() - startNs) / 1000000L
      val label  = f"[${idx + 1}%3d/${proxies.size}%3d] ${proxy.redacted}"

      respE match {
        case Failure(e) =>
          val msg = s"$label ERROR after ${tookMs}ms: ${e.getMessage}"
          println(msg)
          failures += msg

        case Success(resp) if resp.status / 100 != 2 =>
          val snippet = resp.body.take(240).replaceAll("\\s+", " ")
          val msg     = s"$label HTTP ${resp.status} after ${tookMs}ms bodySnippet=$snippet"
          println(msg)
          failures += msg

        case Success(resp) =>
          val jsonE = Try(Json.parse(resp.body))
          jsonE match {
            case Failure(e) =>
              val snippet = resp.body.take(240).replaceAll("\\s+", " ")
              val msg =
                s"$label HTTP ${resp.status} after ${tookMs}ms JSON_PARSE_ERROR=${e.getMessage} bodySnippet=$snippet"
              println(msg)
              failures += msg

            case Success(json) =>
              val venues = (json \ "results" \ "venues").asOpt[JsArray].map(_.value.size).getOrElse(0)
              val msg    = s"$label OK HTTP ${resp.status} after ${tookMs}ms venues=$venues"
              println(msg)
          }
      }
    }

    val errs = failures.result()
    if (errs.nonEmpty) {
      fail(
        s"${errs.size}/${proxies.size} proxies failed the /find check.\n" +
          errs.mkString("\n")
      )
    }
  }
}

