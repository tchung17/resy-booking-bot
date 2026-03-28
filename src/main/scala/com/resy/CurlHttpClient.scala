package com.resy

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters._

final case class CurlHttpResponse(status: Int, headers: String, body: String)

final class CurlHttpClient(proxy: Option[ResyProxy]) {
  def request(
    method: String,
    url: String,
    headers: Seq[(String, String)],
    body: Option[String],
    timeout: FiniteDuration
  ): CurlHttpResponse = {
    val headersFile = Files.createTempFile("resy", ".headers")
    val bodyFile    = Files.createTempFile("resy", ".body")

    try {
      val baseCmd = Vector.newBuilder[String]
      baseCmd += "curl"
      baseCmd += "-sS"
      baseCmd += "--max-time"
      baseCmd += math.max(1L, timeout.toSeconds).toString
      baseCmd += "-D"
      baseCmd += headersFile.toString
      baseCmd += "-o"
      baseCmd += bodyFile.toString
      baseCmd += "-w"
      baseCmd += "%{http_code}"
      baseCmd += "-X"
      baseCmd += method

      proxy.foreach { p =>
        baseCmd += "-x"
        baseCmd += s"http://${p.host}:${p.port}"
        (p.username, p.password) match {
          case (Some(u), Some(pass)) =>
            baseCmd += "--proxy-user"
            baseCmd += s"$u:$pass"
          case _ => ()
        }
      }

      headers.foreach { case (k, v) =>
        baseCmd += "-H"
        baseCmd += s"$k: $v"
      }

      body.foreach { b =>
        baseCmd += "--data-raw"
        baseCmd += b
      }

      baseCmd += url

      val pb = new ProcessBuilder(baseCmd.result().asJava)
      val proc = pb.start()

      val statusStr = new String(proc.getInputStream.readAllBytes(), StandardCharsets.UTF_8).trim
      val stderr    = new String(proc.getErrorStream.readAllBytes(), StandardCharsets.UTF_8).trim
      val exitCode  = proc.waitFor()

      if (exitCode != 0) {
        val msg = if (stderr.nonEmpty) stderr else s"curl exit code $exitCode"
        throw new RuntimeException(msg)
      }

      val status = statusStr.toIntOption.getOrElse {
        throw new RuntimeException(s"curl returned non-numeric status: '$statusStr'")
      }

      val headerText = Files.readString(headersFile, StandardCharsets.UTF_8)
      val bodyText   = Files.readString(bodyFile, StandardCharsets.UTF_8)
      CurlHttpResponse(status = status, headers = headerText, body = bodyText)
    } finally {
      try Files.deleteIfExists(headersFile)
      catch { case _: Throwable => () }
      try Files.deleteIfExists(bodyFile)
      catch { case _: Throwable => () }
    }
  }
}

