package com.resy

import org.joda.time.DateTime
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ResyBookingBotScheduleSpec extends AnyFlatSpec with Matchers {

  behavior of "ResyBookingBot.computeNextSnipeTime"

  it should "schedule for today when snipe time is still in the future" in {
    val now = new DateTime(2026, 1, 1, 8, 0, 0, 0)
    val next = ResyBookingBot.computeNextSnipeTime(now, SnipeTime(hours = 9, minutes = 0), runNow = false)
    next shouldEqual new DateTime(2026, 1, 1, 9, 0, 0, 0)
  }

  it should "schedule for tomorrow when today's snipe time has passed" in {
    val now = new DateTime(2026, 1, 1, 10, 0, 0, 0)
    val next = ResyBookingBot.computeNextSnipeTime(now, SnipeTime(hours = 9, minutes = 0), runNow = false)
    next shouldEqual new DateTime(2026, 1, 2, 9, 0, 0, 0)
  }

  it should "return now when run-now is enabled" in {
    val now = new DateTime(2026, 1, 1, 10, 0, 0, 0)
    val next = ResyBookingBot.computeNextSnipeTime(now, SnipeTime(hours = 9, minutes = 0), runNow = true)
    next shouldEqual now
  }
}

