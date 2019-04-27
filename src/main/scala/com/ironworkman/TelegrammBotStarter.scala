package com.ironworkman

import com.ironworkman.db.{CategoryRepository, UserRepository, WorkPeriodRepository, WorkPeriodsDaysAndTimesRepository}
import com.ironworkman.telegramm.OldIronWorkManBot
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.CommandLineRunner
import org.springframework.stereotype.Service

@Service
class TelegrammBotStarter @Autowired()(userRepository: UserRepository,
                                       workPeriodRepository: WorkPeriodRepository,
                                       workPeriodsDaysAndTimesRepository: WorkPeriodsDaysAndTimesRepository,
                                       categoryRepository: CategoryRepository)
  extends CommandLineRunner {

  override def run(args: String*): Unit = new OldIronWorkManBot(userRepository,
                                                            workPeriodRepository,
                                                            workPeriodsDaysAndTimesRepository,
                                                            categoryRepository).run()
}
