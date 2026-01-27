package com.resy

import akka.actor.ActorSystem
import org.apache.logging.log4j.scala.Logging
import org.joda.time.DateTime
import pureconfig.ConfigSource
import pureconfig.generic.auto._
import scopt.OParser

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import java.nio.file.{Files, Path, Paths}

object ResyBookingBot extends Logging {

  final case class CliOptions(
    configPath: Option[Path] = None,
    runNow: Boolean = false,
    noBook: Boolean = false,
    findOnly: Boolean = false,
    validateConfig: Boolean = false,
    printNextRun: Boolean = false
  )

  private def warnDeprecated(flag: String, replacement: String): Unit =
    logger.warn(s"Flag `$flag` is deprecated; use `$replacement`.")

  private def die(message: String, exitCode: Int): Nothing = {
    System.err.println(message)
    System.exit(exitCode)
    throw new RuntimeException(message)
  }

  private def configSourceFrom(options: CliOptions): ConfigSource = options.configPath match {
    case Some(path) =>
      if (!Files.exists(path)) die(s"Config file not found: $path", 2)
      ConfigSource.file(path)
    case None =>
      ConfigSource.resources("resyConfig.conf")
  }

  private[resy] def computeNextSnipeTime(now: DateTime, snipeTime: SnipeTime, runNow: Boolean): DateTime = {
    if (runNow) now
    else {
      val todays = now
        .withHourOfDay(snipeTime.hours)
        .withMinuteOfHour(snipeTime.minutes)
        .withSecondOfMinute(0)
        .withMillisOfSecond(0)

      if (todays.getMillis > now.getMillis) todays else todays.plusDays(1)
    }
  }

  def main(args: Array[String]): Unit = {
    val builder = OParser.builder[CliOptions]
    val parser = {
      import builder._
      OParser.sequence(
        programName("ResyBookingBot"),
        head("ResyBookingBot"),
        opt[String]("config")
          .abbr("c")
          .valueName("<path>")
          .action((path, c) => c.copy(configPath = Some(Paths.get(path))))
          .text("Path to a resyConfig.conf file (defaults to bundled resource)"),
        opt[String]("config-file")
          .hidden()
          .valueName("<path>")
          .action { (path, c) =>
            warnDeprecated("--config-file", "--config")
            c.copy(configPath = Some(Paths.get(path)))
          },
        opt[Unit]("run-now")
          .action((_, c) => c.copy(runNow = true))
          .text("Run immediately (no sleep)"),
        opt[Unit]("runNow")
          .hidden()
          .action { (_, c) =>
            warnDeprecated("--runNow", "--run-now")
            c.copy(runNow = true)
          },
        opt[Unit]("now")
          .hidden()
          .action { (_, c) =>
            warnDeprecated("--now", "--run-now")
            c.copy(runNow = true)
          },
        opt[Unit]("no-book")
          .action((_, c) => c.copy(noBook = true))
          .text("Run workflow but do not book"),
        opt[Unit]("noBook")
          .hidden()
          .action { (_, c) =>
            warnDeprecated("--noBook", "--no-book")
            c.copy(noBook = true)
          },
        opt[Unit]("dry-run")
          .hidden()
          .action { (_, c) =>
            warnDeprecated("--dry-run", "--no-book")
            c.copy(noBook = true)
          },
        opt[Unit]("dryRun")
          .hidden()
          .action { (_, c) =>
            warnDeprecated("--dryRun", "--no-book")
            c.copy(noBook = true)
          },
        opt[Unit]("find-only")
          .action((_, c) => c.copy(findOnly = true))
          .text("Log availability summary and exit"),
        opt[Unit]("findOnly")
          .hidden()
          .action { (_, c) =>
            warnDeprecated("--findOnly", "--find-only")
            c.copy(findOnly = true)
          },
        opt[Unit]("list")
          .hidden()
          .action { (_, c) =>
            warnDeprecated("--list", "--find-only")
            c.copy(findOnly = true)
          },
        opt[Unit]("inspect")
          .hidden()
          .action { (_, c) =>
            warnDeprecated("--inspect", "--find-only")
            c.copy(findOnly = true)
          },
        opt[Unit]("validate-config")
          .action((_, c) => c.copy(validateConfig = true))
          .text("Validate config and exit"),
        opt[Unit]("print-next-run")
          .action((_, c) => c.copy(printNextRun = true))
          .text("Print computed next run time and exit"),
        help("help").abbr("h").text("Print this help text")
      )
    }

    val options = OParser.parse(parser, args, CliOptions()).getOrElse {
      System.exit(2)
      CliOptions()
    }

    logger.info("Starting Resy Booking Bot")

    val resyConfig = configSourceFrom(options)
    val resyKeysE  = resyConfig.at("resyKeys").load[ResyKeys]
    val resDetailsE = resyConfig.at("resDetails").load[ReservationDetails]
    val snipeTimeE  = resyConfig.at("snipeTime").load[SnipeTime]

    val resyKeys = resyKeysE match {
      case Right(v) => v
      case Left(failures) =>
        die(s"Invalid config at `resyKeys`: ${failures.prettyPrint()}", 2)
    }
    val resDetails = resDetailsE match {
      case Right(v) => v
      case Left(failures) =>
        die(s"Invalid config at `resDetails`: ${failures.prettyPrint()}", 2)
    }
    val snipeTime = snipeTimeE match {
      case Right(v) => v
      case Left(failures) =>
        die(s"Invalid config at `snipeTime`: ${failures.prettyPrint()}", 2)
    }

    if (options.validateConfig) {
      println("Config OK")
      return
    }

    if (resyKeys.apiKey.toLowerCase.contains("resyapi") || resyKeys.apiKey.toLowerCase.contains("api_key"))
      logger.info(
        "Config warning: `resyKeys.api-key` should be the raw key, not the full Authorization header value (e.g. not `ResyAPI api_key=\"...\"`)."
      )

    val dateTimeNow = DateTime.now
    val nextSnipeTime = computeNextSnipeTime(dateTimeNow, snipeTime, options.runNow)

    if (options.printNextRun) {
      val millisUntil =
        if (options.runNow) 0L
        else math.max(0L, nextSnipeTime.getMillis - dateTimeNow.getMillis - 2000)

      println(s"nextSnipeTime=$nextSnipeTime")
      println(s"millisUntilSnipe=$millisUntil")
      return
    }

    val resyApi = new ResyApi(resyKeys)

    val ntfyUrl =
      sys.env.getOrElse("NTFY_URL", "https://ntfy.sh/tim-claude-alerts-1c9d3e")
    val ntfyEnabled =
      sys.env.get("NTFY_ENABLED").forall(v => v != "0" && v.toLowerCase != "false")
    val ntfy = new NtfyClient(NtfyClientSettings(url = ntfyUrl, enabled = ntfyEnabled))
    val events: ResyEventSink = new NtfyResyEventSink(ntfy, global)

    val runInfo = ResyRunInfo(
      proxy        = ResyApi.selectedProxy,
      configPath   = options.configPath.map(_.toString).getOrElse("classpath:resyConfig.conf"),
      date         = resDetails.date,
      partySize    = resDetails.partySize,
      venueId      = resDetails.venueId,
      resTimeTypes = resDetails.resTimeTypes,
      snipeTime    = snipeTime,
      findOnly     = options.findOnly,
      runNow       = options.runNow,
      noBook       = options.noBook,
      tokenExpiry  = ResyDiagnostics.jwtExpiry(resyKeys.authToken)
    )
    events.botStarted(runInfo)

    val resyClient          = new ResyClient(resyApi, events = events, runInfo = Some(runInfo))
    val resyBookingWorkflow = new ResyBookingWorkflow(resyClient, resDetails)

    if (options.findOnly) {
      resyClient.logFindSummary(resDetails.date, resDetails.partySize, resDetails.venueId) match {
        case scala.util.Success(_) => ()
        case scala.util.Failure(e) =>
          die(s"Find endpoint failed: ${e.getMessage}", 4)
      }
      Await.result(ResyApi.shutdown(), 10.seconds)
      return
    }

    val configPathStr =
      options.configPath.map(_.toString).getOrElse("classpath:resyConfig.conf")
    val tokenExpStr =
      ResyDiagnostics.jwtExpiry(resyKeys.authToken).map(_.toString).getOrElse("unknown")

    logger.info(
      s"Target: date=${resDetails.date} partySize=${resDetails.partySize} venueId=${resDetails.venueId} " +
        s"resTimeTypes=${ResyDiagnostics.formatResTimeTypes(resDetails.resTimeTypes)} config=$configPathStr"
    )
    logger.info(s"Auth token exp: $tokenExpStr")
    logger.info("Startup check: querying availability now (find endpoint)")
    resyClient.logFindSummary(resDetails.date, resDetails.partySize, resDetails.venueId) match {
      case scala.util.Success(_) => ()
      case scala.util.Failure(e) =>
        // Don't sleep for hours if our credentials/network/etc are broken.
        die(s"Startup check failed (find endpoint): ${e.getMessage}", 4)
    }

    val system      = ActorSystem("System")

    val millisUntilSnipe =
      if (options.runNow) 0L
      else math.max(0L, nextSnipeTime.getMillis - DateTime.now.getMillis - 2000)

    val hoursRemaining      = millisUntilSnipe / 1000 / 60 / 60
    val minutesRemaining    = millisUntilSnipe / 1000 / 60 - hoursRemaining * 60
    val secondsRemaining =
      millisUntilSnipe / 1000 - hoursRemaining * 60 * 60 - minutesRemaining * 60

    if (options.runNow) logger.info("Run-now enabled; skipping sleep and attempting immediately")
    else {
      logger.info(s"Next snipe time: $nextSnipeTime")
      logger.info(
        s"Sleeping for $hoursRemaining hours, $minutesRemaining minutes, and $secondsRemaining seconds"
      )
    }

    system.scheduler.scheduleOnce(millisUntilSnipe millis) {
      if (options.noBook) resyBookingWorkflow.run(doBook = false, millisToRetry = (20 seconds).toMillis)
      else resyBookingWorkflow.run()

      logger.info("Shutting down Resy Booking Bot")
      ResyApi.shutdown()
      system.terminate()
    }

    Await.result(system.whenTerminated, Duration.Inf)
  }
}
