package com.resy

import java.nio.charset.StandardCharsets

import scala.concurrent.{ExecutionContext, Future}

final case class NtfyClientSettings(
  url: String,
  enabled: Boolean = true
)

final class NtfyClient(settings: NtfyClientSettings) {
  def publishAsync(
    title: String,
    message: String,
    tags: Seq[String] = Nil,
    priority: Option[Int] = None
  )(implicit ec: ExecutionContext): Unit = {
    if (!settings.enabled) return
    Future {
      publish(title = title, message = message, tags = tags, priority = priority)
    }.recover { case _: Throwable => () }
    ()
  }

  private def publish(
    title: String,
    message: String,
    tags: Seq[String],
    priority: Option[Int]
  ): Unit = {
    if (!settings.enabled) return

    val cmd = Vector.newBuilder[String]
    cmd += "curl"
    cmd += "-sS"
    cmd += "-o"
    cmd += "/dev/null"
    cmd += "-w"
    cmd += "%{http_code}"
    cmd += "-X"
    cmd += "POST"
    cmd += "-H"
    cmd += s"Title: $title"
    if (tags.nonEmpty) {
      cmd += "-H"
      cmd += s"Tags: ${tags.mkString(",")}"
    }
    priority.foreach { p =>
      cmd += "-H"
      cmd += s"Priority: $p"
    }
    cmd += "--data-binary"
    cmd += message
    cmd += settings.url

    val pb   = new ProcessBuilder(cmd.result(): _*)
    val proc = pb.start()
    val statusStr =
      new String(proc.getInputStream.readAllBytes(), StandardCharsets.UTF_8).trim
    proc.getErrorStream.readAllBytes() // drain
    proc.waitFor()

    // Best effort: ignore non-2xx. If you want stricter behavior, log here.
    val status = statusStr.toIntOption.getOrElse(0)
    if (status / 100 != 2) ()
  }
}

