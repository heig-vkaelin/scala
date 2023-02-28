def hello: Unit = 
  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"

val test = "Hello world!"

val number=10

1.to(10)

def square (x:Int) = x*x

val num = 3.14
val fun = math.ceil _
fun(num)

(x: Int) => x*x

for i <- 0.to(10) yield i


val map = Map(1 -> "one", 2 -> "two", 3 -> "three")

map.getOrElse(4, map.getOrElse(5, map.getOrElse(6, "ntm")))

def guess(l: List[Int]): List[Int] = l match {
case Nil => Nil
case x::xs if x < 10 => guess(xs)
case x::xs if x >= 10 => x :: guess(xs)
}
