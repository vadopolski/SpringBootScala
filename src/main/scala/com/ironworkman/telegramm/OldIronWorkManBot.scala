package com.ironworkman.telegramm

import com.ironworkman.db._
import info.mukel.telegrambot4s.models.Message

import scala.language.postfixOps

import info.mukel.telegrambot4s._
import api._
import methods._
import Implicits._
import cats.effect.{IO, Timer}

import scala.language.postfixOps

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.postfixOps

class OldIronWorkManBot(val userRepository: UserRepository,
                        val workPeriodRepository: WorkPeriodRepository,
                        val workPeriodsDaysAndTimesRepository: WorkPeriodsDaysAndTimesRepository)
    extends TelegramBot with Polling with Commands {

  implicit val timer                                     = IO.timer(ExecutionContext.global)
  def token                                              = "767996938:AAF6talqUn--PI0z2vJeAxcOtvMRWrQkevw"
  def sendMessageIO(chatID: Long, msg: String): IO[Unit] = IO(request(SendMessage(chatID, msg)))
  def cleanAndToLong(input: String): Long                = input.replace(" ", "").toLong

  def scheduler(count: Long, amount: Long, duration: Long, chatId: Long): IO[Unit] =
    for {
      _ <- amount - count match {
            case 0 =>
              for {
                _ <- respond(s"Your work is finished after $count intervals", chatId)
                // TODO: Reading from database statistics
              } yield ()
            case _ =>
              for {
                _ <- respond(s"The $count interval started, work!", chatId)
                _ <- Timer[IO].sleep(duration second)
                _ <- respond(s"Write please time for 1,2,3 categories of $count interval", chatId)
                _ <- respond(s"Example: /time 15 10 5 programming drink tea mail", chatId)
                _ <- Timer[IO].sleep(15 second)
                _ <- scheduler(count + 1, amount, duration, chatId)
              } yield ()
          }
    } yield ()

  def respond(msg: String, chatID: Long): IO[Unit] =
    for {
      _ <- sendMessageIO(chatID, msg)
    } yield ()

  def greeting(chatId: Long, userId: Long, userName: String): IO[Unit] =
    for {
      _ <- respond(s"Hello ${userName}! I`m IronWorkMan bot.", chatId)
      _ <- respond(s"Please enter a command with the parameters of the number of " +
                     s"intervals and the duration of the interval in minutes.",
                   chatId)
      _ <- respond(s"For Example /start 5 30", chatId)
      _ <- respond(s"A sprint of 5 intervals of 30 minutes will start.", chatId)
      _ <- IO(userRepository.save(User(userId, userName)))
    } yield ()

  def startSprint(chatId: Long, userId: Long, userName: String, duration: Long, amount: Long): IO[Unit] =
    for {
      _ <- respond(s"A sprint of $amount intervals of $duration minutes started.", chatId)
      _ <- IO(
            workPeriodsDaysAndTimesRepository
              .save(WorkPeriodsDaysAndTimes(chatId, amount, duration, User(userId, userName))))
      _ <- scheduler(0, amount.toLong, duration.toLong, chatId)
    } yield ()

  override def onMessage(message: Message) = message.text match {
    case Some(text) if text.contains("start") =>
      text.split(" ") match {
        case Array(_, amount, duration) =>
          startSprint(message.chat.id, message.from.get.id, message.from.get.username.get, amount.toLong, duration.toLong)
            .unsafeRunAsyncAndForget()
        case _ => respond("Enter command and two parameters, please", message.chat.id)
      }
    case Some(text) => greeting(message.chat.id, message.from.get.id, message.from.get.username.get).unsafeRunAsyncAndForget()
  }
}
