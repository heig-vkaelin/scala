import scala.util.{Failure, Random, Success}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

// 1.
var durations = Iterator.continually(Random.nextInt(100))
var successes = Iterator.continually(Random.nextDouble() > 0.7)

def f(name: String): Future[(Int, String)] = {
  val duration = durations.next()
  val success = successes.next()
  println (s"creating future $name")
  val f = Future {
    Thread.sleep(duration)
    if (success) (duration, name) else throw new Exception(s"did not succeed")
  }
  f.onComplete {
    case Success((duration, name)) => println(s"success for $name with value $duration")
    case Failure(exception) => println(s"$name failed with $exception")
  }
  f
}

// a)
val f1 = f("f1")
val f2 = f("f2")
val f3 = f("f3")

// b)
val f4 = f("f4")
val f5 = f4.flatMap(_ => f("f5"))

// c)
val f6 = f("f6")
val f7 = f6.filter(_._1 > 700).flatMap(_ => f("f7"))
val f8 = f7.flatMap(_ => f("f8"))

// Variante avec for comprehension
// val v2 = for {
//   f6 <- f("f6")
//   if f6._1 > 700
//   f7 <- f("f7")
//   f8 <- f("f8")
// } yield f8._1

// d)
val list = Future.sequence(List(f("f9"), f("f10"), f("f11"))).onComplete {
  case Success(value) =>
    println(s"success for summed list with value ${value.map(_._1).sum}")
  case Failure(exception) =>
    println(s"list failed with exception $exception")
}

// e)
val f12 = f("f12")
val f13 = f("f13")
val f14 = f("f14")

val f12f13f14 = Future.firstCompletedOf(List(f12, f13, f14)).map {
  case (duration, name) => println(s"$name a terminé en premier")
}

// f)
val f15 = f("f15")
val f16 = f15.recoverWith {
  case _ => f("f16")
}

Thread.sleep(400)

// 2.
def exists[T](f:Future[T])(p: T => Boolean) : Future [Boolean] = 
  f.map(p).recover { case _ => false }

val f17 = f("f17")
val result = exists(f17)(_._1 > 700)
val result2 = exists(f17)(_._1 < 700)

Thread.sleep(400)

// 3.
def getWeather(url: String): Future[String] = 
  val success = successes.next()

  Future {
    Thread.sleep(100)
    val weather = List("sunny", "rainy", "windy", "snowy")
    if (success) weather(Random.nextInt(weather.size)) else throw new Exception(s"$url did not succeed")
  }

val urls: List[String] = List("Paris", "London", "New-York", "Tokyo")

def successiveFallbacks(urls: List[String]): Future[String] = 
  urls match {
    case Nil => Future.failed(new Exception("No urls"))
    case url :: Nil => getWeather(url)
    case url :: tail => getWeather(url).fallbackTo(successiveFallbacks(tail))
  }

successiveFallbacks(urls).onComplete {
  case Success(value) => println(s"success, weater: $value")
  case Failure(exception) => println(s"failure for $exception")
}

Thread.sleep(400)
