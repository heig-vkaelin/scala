@main def hello: Unit = 
  val x = 0
  recursive(x)

def msg = "I was compiled by Scala 3. :)"


def recursive(x: Int): Int = {
  println(x)
  if (x > 100) return x
  if (x % 7 == 0) return recursive(x + 8)
  if (x % 2 != 0) return recursive(x + 12)
  return recursive(x + 1)
}
