package com.resy

import scala.concurrent.ExecutionContext

final class NtfyResyEventSink(
  ntfy: NtfyClient,
  implicit val ec: ExecutionContext
) extends ResyEventSink {

  override def botStarted(info: ResyRunInfo): Unit = {
    val body =
      Seq(
        s"proxy=${info.proxyLabel}",
        s"config=${info.configPath}",
        s"date=${info.date}",
        s"partySize=${info.partySize}",
        s"venueId=${info.venueId}",
        s"resTimeTypes=${ResyDiagnostics.formatResTimeTypes(info.resTimeTypes)}",
        s"snipeTime=%02d:%02d".format(info.snipeTime.hours, info.snipeTime.minutes),
        s"findOnly=${info.findOnly}",
        s"runNow=${info.runNow}",
        s"noBook=${info.noBook}",
        s"authTokenExp=${info.tokenExpiry.map(_.toString).getOrElse("unknown")}"
      ).mkString("\n")

    ntfy.publishAsync(
      title   = "Resy bot started",
      message = body,
      tags    = Seq("resy", "boot"),
      priority = Some(3)
    )
  }

  override def findSummary(info: ResyRunInfo, summary: ResyClient.FindSummary): Unit = {
    val venue = summary.venueName.getOrElse(s"venueId=${info.venueId}")
    val suffix = if (summary.times.size > 10) ",..." else ""
    val timesStr =
      if (summary.times.isEmpty) "(none)"
      else summary.times.take(10).mkString(",") + suffix

    val body =
      Seq(
        s"proxy=${info.proxyLabel}",
        s"venue=$venue",
        s"date=${info.date}",
        s"partySize=${info.partySize}",
        s"slots=${summary.slotCount}",
        s"times=$timesStr"
      ).mkString("\n")

    ntfy.publishAsync(
      title   = "Resy startup find",
      message = body,
      tags    = Seq("resy", "find", "startup"),
      priority = Some(3)
    )
  }

  override def findAttempt(info: ResyRunInfo, attempt: ResyFindAttemptInfo): Unit = {
    val venue = attempt.venueName.getOrElse(s"venueId=${info.venueId}")

    val times =
      attempt.times.take(10).map { t =>
        val types = attempt.timeToTableTypes.getOrElse(t, Nil).take(6)
        if (types.isEmpty) t else s"$t(${types.mkString(",")})"
      }

    val suffix = if (attempt.times.size > 10) ",..." else ""
    val timesStr = if (times.isEmpty) "(none)" else times.mkString(",") + suffix

    val baseLines =
      Seq(
        s"proxy=${info.proxyLabel}",
        s"attempt=${attempt.attempt}",
        s"venue=$venue",
        s"date=${info.date}",
        s"partySize=${info.partySize}",
        s"slots=${attempt.slotCount}",
        s"times=$timesStr"
      )

    val lines = attempt.error match {
      case None    => baseLines
      case Some(e) => baseLines :+ s"error=$e"
    }

    ntfy.publishAsync(
      title   = "Resy find attempt",
      message = lines.mkString("\n"),
      tags    = Seq("resy", "attempt"),
      priority = Some(2)
    )
  }
}
