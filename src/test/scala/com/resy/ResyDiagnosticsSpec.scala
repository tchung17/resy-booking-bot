package com.resy

import org.joda.time.DateTime
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ResyDiagnosticsSpec extends AnyFlatSpec with Matchers {

  behavior of "ResyDiagnostics.jwtExpiry"

  it should "decode exp from a JWT-shaped token" in {
    // header/payload/signature where payload is {"exp": 1700000000}
    val token = "x.eyJleHAiOiAxNzAwMDAwMDAwfQ.y"
    ResyDiagnostics.jwtExpiry(token) shouldEqual Some(new DateTime(1700000000L * 1000L))
  }

  it should "return None for non-JWT tokens" in {
    ResyDiagnostics.jwtExpiry("not-a-jwt") shouldEqual None
  }
}

