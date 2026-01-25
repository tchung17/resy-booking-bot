package com.resy

import org.joda.time.DateTime

final case class ResyRunInfo(
  proxy: Option[ResyProxy],
  configPath: String,
  date: String,
  partySize: Int,
  venueId: Int,
  resTimeTypes: Seq[ReservationTimeType],
  snipeTime: SnipeTime,
  findOnly: Boolean,
  runNow: Boolean,
  noBook: Boolean,
  tokenExpiry: Option[DateTime]
) {
  def proxyLabel: String = proxy.map(_.redacted).getOrElse("none")
}

final case class ResyFindAttemptInfo(
  attempt: Int,
  venueName: Option[String],
  slotCount: Int,
  times: Seq[String],
  timeToTableTypes: Map[String, Seq[String]],
  error: Option[String] = None
)

trait ResyEventSink {
  def botStarted(info: ResyRunInfo): Unit
  def findSummary(info: ResyRunInfo, summary: ResyClient.FindSummary): Unit
  def findAttempt(info: ResyRunInfo, attempt: ResyFindAttemptInfo): Unit
}

object ResyEventSink {
  val noop: ResyEventSink = new ResyEventSink {
    override def botStarted(info: ResyRunInfo): Unit = ()
    override def findSummary(info: ResyRunInfo, summary: ResyClient.FindSummary): Unit = ()
    override def findAttempt(info: ResyRunInfo, attempt: ResyFindAttemptInfo): Unit = ()
  }
}
