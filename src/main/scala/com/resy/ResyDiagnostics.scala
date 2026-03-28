package com.resy

import org.joda.time.DateTime
import play.api.libs.json.Json

import java.util.Base64
import scala.util.Try

object ResyDiagnostics {

  /** Best-effort decode of JWT `exp` (epoch seconds). Returns None if token isn't JWT-shaped. */
  def jwtExpiry(authToken: String): Option[DateTime] = {
    val parts = authToken.split("\\.", -1)
    if (parts.length < 2) return None

    val payloadB64 = parts(1)
    val padded =
      payloadB64 + ("=" * ((4 - (payloadB64.length % 4)) % 4))

    val payloadJson = Try {
      val bytes = Base64.getUrlDecoder.decode(padded)
      new String(bytes, "UTF-8")
    }.toOption

    payloadJson.flatMap { s =>
      Try((Json.parse(s) \ "exp").as[Long]).toOption.map(sec => new DateTime(sec * 1000L))
    }
  }

  def formatResTimeTypes(resTimeTypes: Seq[ReservationTimeType]): String =
    resTimeTypes
      .map { rtt =>
        rtt.tableType.filter(_.nonEmpty) match {
          case Some(tt) => s"${rtt.reservationTime}($tt)"
          case None     => rtt.reservationTime
        }
      }
      .mkString(",")
}

