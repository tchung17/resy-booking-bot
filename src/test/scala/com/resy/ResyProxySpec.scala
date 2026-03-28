package com.resy

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ResyProxySpec extends AnyFunSuite with Matchers {
  test("parse host:port") {
    ResyProxy.parse("proxy.example.com:8080") shouldBe Some(
      ResyProxy(host = "proxy.example.com", port = 8080, username = None, password = None)
    )
  }

  test("parse username:password@host:port") {
    ResyProxy.parse("user:pass@proxy.example.com:3128") shouldBe Some(
      ResyProxy(host = "proxy.example.com", port = 3128, username = Some("user"), password = Some("pass"))
    )
  }

  test("parse http://username:password@host:port") {
    ResyProxy.parse("http://user:pass@proxy.example.com:3128") shouldBe Some(
      ResyProxy(host = "proxy.example.com", port = 3128, username = Some("user"), password = Some("pass"))
    )
  }

  test("parseMany splits on whitespace and commas") {
    ResyProxy.parseMany("a:1, b:2\nc:3") shouldBe Seq(
      ResyProxy(host = "a", port = 1, username = None, password = None),
      ResyProxy(host = "b", port = 2, username = None, password = None),
      ResyProxy(host = "c", port = 3, username = None, password = None)
    )
  }

  test("fromEnv prefers pool keys and returns one entry") {
    val env = Map("RESY_PROXY_POOL" -> "a:1 b:2")
    val selected = ResyProxy.fromEnv(env)
    selected.map(_.host) should (be(Some("a")) or be(Some("b")))
  }

  test("fromEnv prefers RESY_PROXY over pool") {
    val env = Map("RESY_PROXY_POOL" -> "a:1 b:2", "RESY_PROXY" -> "c:3")
    ResyProxy.fromEnv(env) shouldBe Some(
      ResyProxy(host = "c", port = 3, username = None, password = None)
    )
  }
}
