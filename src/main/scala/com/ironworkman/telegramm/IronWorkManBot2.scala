package com.ironworkman.telegramm

import info.mukel.telegrambot4s.models.Message

import scala.language.postfixOps
import info.mukel.telegrambot4s._
import api._
import methods._
import Implicits._
import cats.effect.Timer
import cats.implicits._
import monix.execution.Scheduler

import scala.language.postfixOps
import scala.concurrent.duration._
import scala.language.postfixOps
import java.util.concurrent.Executors

import cats.effect.{ContextShift, IO}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.control.NonFatal


object IronWorkManBot2 extends TelegramBot with Polling with Commands {
  implicit val timer = IO.timer(ExecutionContext.global)
  implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  private val io = Scheduler.io(name="engine-io")
  val blockingEC = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  def executeBlockingIO[T](cb: => T): Future[T] = {
    val p = Promise[T]()

    io.execute(new Runnable {
      def run() = try {
        p.success(cb)
      }
      catch {
        case NonFatal(ex) =>
          logger.error(s"Uncaught I/O exception", ex)
          p.failure(ex)
      }
    })

    p.future
  }

  def token = "767996938:AAF6talqUn--PI0z2vJeAxcOtvMRWrQkevw"

  def blockingOp1: IO[Unit] = IO(println("blocking 1"))
  def blockingOp2: IO[Unit] = IO(println("blocking 2"))
  def doSth(): IO[Unit] = IO(/* do something */ ())


  def prog: IO[Unit] =
    contextShift.evalOn(blockingEC)(blockingOp1) *>
      Timer[IO].sleep(5 second) *>
      IO(executeBlockingIO(blockingOp2)) *>
      doSth()

  def greeting(chatId: Long): IO[Unit] =
    for {
      _ <- IO.fromFuture(IO(request(SendMessage(chatId, "first"))))
      _ <- IO.fromFuture(IO(request(SendMessage(chatId, "second"))))
    } yield ()

  override def onMessage(message: Message) = message.text match {
    case Some(_) => greeting(message.chat.id).unsafeRunAsyncAndForget()
  }

  def main(args: Array[String]): Unit = {
    IronWorkManBot.run()
  }
}