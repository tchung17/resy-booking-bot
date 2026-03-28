package com.resy

final case class ResyProxy(host: String, port: Int, username: Option[String], password: Option[String]) {
  def redacted: String = {
    val userPart = username.filter(_.nonEmpty).map(u => s"${ResyProxy.redact(u)}@").getOrElse("")
    s"$userPart$host:$port"
  }
}

object ResyProxy {
  private val SupportedSchemes = Set("http", "https", "socks5")

  def fromEnv(env: Map[String, String] = sys.env): Option[ResyProxy] = {
    val entries =
      env
        .get("RESY_PROXY")
        .orElse(env.get("RESY_PROXY_POOL"))
        .orElse(env.get("RESY_PROXIES"))
        .toSeq
        .flatMap(parseMany(_))

    if (entries.isEmpty) None
    else Some(entries(scala.util.Random.nextInt(entries.size)))
  }

  def parseMany(value: String): Seq[ResyProxy] =
    value
      .split("[\\s,]+")
      .toSeq
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap(parse)

  def parse(value: String): Option[ResyProxy] = {
    // Accept:
    // - host:port
    // - username:password@host:port
    // - http(s)://username:password@host:port
    // - socks5://host:port
    val trimmed = value.trim
    if (trimmed.isEmpty) None
    else {
      val withoutScheme =
        trimmed.indexOf("://") match {
          case -1 => trimmed
          case idx =>
            val scheme = trimmed.substring(0, idx).toLowerCase
            if (!SupportedSchemes.contains(scheme)) return None
            trimmed.substring(idx + 3)
        }

      val atIdx = withoutScheme.lastIndexOf('@')
      val (credsOpt, hostPort) =
        if (atIdx >= 0) (Some(withoutScheme.substring(0, atIdx)), withoutScheme.substring(atIdx + 1))
        else (None, withoutScheme)

      val hpParts = hostPort.split(":", 2)
      if (hpParts.length != 2) return None
      val host = hpParts(0).trim
      val port = hpParts(1).trim.toIntOption.getOrElse(return None)
      if (host.isEmpty || port <= 0 || port > 65535) return None

      val (user, pass) = credsOpt match {
        case None => (None, None)
        case Some(creds) =>
          val parts = creds.split(":", 2)
          if (parts.length != 2) return None
          (Some(parts(0)), Some(parts(1)))
      }

      Some(ResyProxy(host = host, port = port, username = user, password = pass))
    }
  }

  private[resy] def redact(value: String): String =
    if (value.length <= 8) "********"
    else value.take(4) + "…" + value.takeRight(4)
}
