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

    def main: Unit = {

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


}
