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
import com.ironworkman.db._

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future, Promise}
import scala.util.control.NonFatal

class OldIronWorkManBot(userRepository: UserRepository,
                        workPeriodRepository: WorkPeriodRepository,
                        workPeriodsDaysAndTimesRepository: WorkPeriodsDaysAndTimesRepository,
                        categoryRepository: CategoryRepository)
    extends TelegramBot with Polling with Commands {
  private val executor: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))
  implicit val timer                          = IO.timer(executor)
  implicit val contextShift: ContextShift[IO] = IO.contextShift(executor)

  val blockingEC = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  def token                               = "767996938:AAF6talqUn--PI0z2vJeAxcOtvMRWrQkevw"
  def saveUserIO(userId: Long, userName: String): IO[Unit] = IO(userRepository.save(User(userId, userName)))


  def greeting(chatId: Long, userId: Long, userName: String): IO[Unit] =
    for {
      _ <- sendMessageMethodIO(s"Hello ${userName}! I`m IronWorkMan bot.", chatId)
      _ <- sendMessageMethodIO(s"Please enter a command with the parameters of the number of " +
                     s"intervals and the duration of the interval in minutes.",
                   chatId)
      _ <- sendMessageMethodIO(s"For Example /start 5 30", chatId)
      _ <- sendMessageMethodIO(s"A sprint of 5 intervals of 30 minutes will start.", chatId)
      _ <- contextShift.evalOn(blockingEC)(saveUserIO(userId, userName))
    } yield ()

  def startSprint(chatId: Long, userId: Long, userName: String, duration: Long, amount: Long): IO[Unit] =
    for {
      _ <- sendMessageMethodIO(s"A sprint of $amount intervals of $duration minutes started.", chatId)
      _ <- IO(
            workPeriodsDaysAndTimesRepository
              .save(WorkPeriodsDaysAndTimes(chatId, amount, duration, User(userId, userName))))
      _ <- scheduler(0, amount.toLong, duration.toLong, chatId)
    } yield ()

  def scheduler(count: Long, amount: Long, duration: Long, chatId: Long): IO[Unit] =
    for {
      _ <- amount - count match {
            case 0 =>
              for {
                _ <- sendMessageMethodIO(s"Your sprint is finished after $count intervals", chatId)
                _ <- IO(workPeriodRepository.findSumByCategoryIdAndUser2(1))
                      .flatMap(paid => sendMessageMethodIO(s"You were paid for $paid minutes", chatId))
                _ <- IO(workPeriodRepository.findSumByCategoryIdAndUser2(2))
                      .flatMap(dontStopPaying => sendMessageMethodIO(s"You did not stop paying for $dontStopPaying minutes", chatId))
                _ <- IO(workPeriodRepository.findSumByCategoryIdAndUser2(3))
                      .flatMap(dontPay => sendMessageMethodIO(s"You were not paid for $dontPay minutes", chatId))
              } yield ()
            case _ =>
              for {
                _ <- sendMessageMethodIO(s"The $count interval started, work!", chatId)
                _ <- Timer[IO].sleep(duration second)
                _ <- sendMessageMethodIO(s"Write please time for 1,2,3 categories of $count interval", chatId)
                _ <- sendMessageMethodIO(s"Example: /time 15 10 5 programming drink tea mail", chatId)
                _ <- Timer[IO].sleep(20 second)
                _ <- scheduler(count + 1, amount, duration, chatId)
              } yield ()
          }
    } yield ()

  def recordTime(chatId: Long, paidTime: Long, dontStopPayingTime: Long, notPayingTime: Long, description: String): IO[Unit] =
    for {
      workPeriodsDaysAndTimes <- IO(workPeriodsDaysAndTimesRepository.findById(chatId))
      paid                    <- IO(categoryRepository.findById(1L))
      _ <- IO(
            workPeriodRepository
              .save(WorkPeriod(null, paidTime, description, paid.get, workPeriodsDaysAndTimes.get)))
      _              <- sendMessageMethodIO(s"A time is recorded", chatId)
      dontStopPaying <- IO(categoryRepository.findById(2L))
      _ <- IO(
            workPeriodRepository
              .save(WorkPeriod(null, dontStopPayingTime, description, dontStopPaying.get, workPeriodsDaysAndTimes.get)))
      _         <- sendMessageMethodIO(s"A time is recorded", chatId)
      notPaying <- IO(categoryRepository.findById(3L))
      _ <- IO(
            workPeriodRepository
              .save(WorkPeriod(null, notPayingTime, description, notPaying.get, workPeriodsDaysAndTimes.get)))
      _ <- sendMessageMethodIO(s"A time is recorded", chatId)
    } yield ()

  override def onMessage(message: Message) = message.text match {
    case Some(text) if text.contains("start") =>
      text.split(" ") match {
        case Array(_, amount, duration) =>
          startSprint(message.chat.id, message.from.get.id, message.from.get.username.get, amount.toLong, duration.toLong)
            .unsafeRunAsyncAndForget()
        case _ =>
          sendMessageMethodIO("Enter command /start and two parameters, please", message.chat.id)
            .unsafeRunAsyncAndForget()
      }
    case Some(text) if text.contains("time") =>
      text.split(" ") match {
        case Array(_, paidTime, dontStopPayingTime, notPayingTime, description) =>
          recordTime(message.chat.id, paidTime.toLong, dontStopPayingTime.toLong, notPayingTime.toLong, description)
            .unsafeRunAsyncAndForget()
        case _ =>
          sendMessageMethodIO("Enter command /time and four parameters, please", message.chat.id)
            .unsafeRunAsyncAndForget()
      }
    case Some(_) =>
      greeting(message.chat.id, message.from.get.id, message.from.get.username.get)
        .unsafeRunAsyncAndForget()
  }

  def sendMessageMethodIO(msg: String, chatID: Long): IO[Message] = IO.fromFuture(IO(request(SendMessage(chatID, msg))))
}