package fpmax

import scala.io.StdIn.readLine
import scala.util.{Random, Try}

object App {

  def main(args: Array[String]): Unit = {

    println("what is your name?")

    val name = readLine()

    println(s"hello $name and welcome to the game!")

    var exec = true

    while (exec) {

      val num = Random.nextInt(5) + 1

      println(s"Dear $name, please guess a number from 1 to 5:")

      // the program could crash here, as the cast may not work
      val guess = readLine().toInt

      if (guess == num) {
        println(s"You guessed it right, $name!")
      } else {
        println(s"You guessed wrong, $name! The number was: $num")
      }
      println(s"Do you want to continue, $name?")

      // the program could crash here as well as the match may not be exaustive
      readLine() match {
        case "y" => exec = true
        case "n" => exec = false
      }
    }

  }


  object Intro {
    // fake implementations
    def println(s: String): Unit = ()
    def readLine(): String = "Jack"
    def parseInt(s: String): Int = 123
  }

  object App2 {

    // let's eliminate the risk of casting to Int
    def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

    def main(args: Array[String]): Unit = {

      println("what is your name?")

      val name = readLine()

      println(s"hello $name and welcome to the game!")

      var exec = true
      while (exec) {

        val num = Random.nextInt(5) + 1

        println(s"Dear $name, please guess a number from 1 to 5:")

        val guess = parseInt(readLine())
        guess match {
          case None => println("You did not enter a valid number")
          case Some(value) =>
            if (value == num) {
              println(s"You guessed it right, $name!")
            } else {
              println(s"You guessed wrong, $name! The number was: $num")
            }
        }

        // getting rid of the other point of failure
        var continue = true
        while (continue) {
          continue = false
          println(s"Do you want to continue, $name?")
          readLine().toLowerCase match {
            case "y" => exec = true
            case "n" => exec = false
            case _ => continue = true
          }
        }

      }

    }
  }



  object App3 {

    // let's eliminate the risk of casting to Int
    def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

    // let's introduce IO[A], which is a description of any interaction with the external
    // world that produces an A type.
    // We define a flatmap (and a map), so it is going to be a monadic type

    case class IO[A](unsafeRun: () => A) { self =>

      def map[B](f: A => B): IO[B] = {
        IO(() => f(self.unsafeRun))
      }

      def flatMap[B](f: A => IO[B]): IO[B] = {
        IO(() => f(self.unsafeRun)).unsafeRun()
      }

    }

    // let's also define the wrap of the monad
    object IO {
      def point[A](a: => A): IO[A] = {
        IO(() => a)
      }
    }

    // now we can purify the read/write console functions
    def putStrLn(s: String): IO[Unit] =
      IO(() => println(s))

    def getStrLn: IO[String] =
      IO(() => readLine())

    // utilities
    def nextInt(max: Int): IO[Int] =
      IO(() => Random.nextInt(max))

    def checkContinue(name: String): IO[Boolean] =
      for {
        _         <- putStrLn(s"Do you want to continue, $name?")
        input     <- getStrLn.map(_.toLowerCase)
        continue  <- input match {
          case "y" => IO.point(true)
          case "n" => IO.point(false)
          case _ => checkContinue(name)
        }
      } yield continue


    def gameLoop(name: String): IO[Unit] =

      for {
        num       <- nextInt(5).map(_ + 1)
        _         <- putStrLn(s"Dear $name, please guess a number from 1 to 5:")
        input     <- getStrLn
        _         <- parseInt(input).fold(
          putStrLn("You did not enter a valid number")
        )( value =>
          if (value == num) {
            putStrLn(s"You guessed it right, $name!")
          } else {
            putStrLn(s"You guessed wrong, $name! The number was: $num")
          }
        )
        continue  <- checkContinue(name)
        _         <- if (continue) {
          gameLoop(name)
        } else {
          IO.point(())
        }


      } yield ()


    // now we can call putStrLn and getStrLn safely. We should use flatmap. Or we can use
    // a for comprehension, which reminds of Haskell, and we can write in semi-imperative style
    def main(args: Array[String]): IO[Unit] =

      for {
        _     <- putStrLn("what is your name?")
        name  <- getStrLn
        _     <- putStrLn(s"hello $name and welcome to the game!")
        _     <- gameLoop(name)
      } yield ()




  }

}
