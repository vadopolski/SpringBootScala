package com.ironworkman.telegramm

package com.ironworkman.telegramm

import scala.language.postfixOps
import info.mukel.telegrambot4s._
import api._
import cats.effect.Timer
import cats.implicits._

import scala.language.postfixOps
import scala.concurrent.duration._
import scala.language.postfixOps

import cats.effect.{ContextShift, IO}
import info.mukel.telegrambot4s.models.Message

import scala.concurrent.ExecutionContext

object OldIronWorkManBot3
  extends TelegramBot with Polling with Commands {
  def token          = "767996938:AAF6talqUn--PI0z2vJeAxcOtvMRWrQkevw"
  implicit val timer = IO.timer(ExecutionContext.global)

  def doSth(str: String): IO[Unit] = IO(println(str))
  def greeting(): IO[Unit] =
    doSth("Before timer.") *>
      Timer[IO].sleep(5 second) *>
      doSth("After timer")

//  val a = greeting().unsafeRunAsyncAndForget()

    def main(args: Array[String]): Unit = OldIronWorkManBot3.run()
}
