// 1.
def addList(l: List[Int]): Int = 
  l match
    case Nil => 0
    case x :: xs => x + addList(xs)

def addListTail(l: List[Int]): Int = {
  def loop(l: List[Int], acc: Int): Int = 
    l match
      case Nil => acc
      case x :: xs => loop(xs, acc + x)
  loop(l, 0)
}

addList(List(1, 2, 3, 4, 5))
addListTail(List(1, 2, 3, 4, 5))

// 2.
def func2(x: Int): Int = 
  if (x % 3 == 0) x * x * x
  else if (x % 2 == 0) x * x
  else -1

val v1 = (1 to 1000).toVector
v1.map(func2)

// 3.
def factorial(n: Int) =
  def loop(acc: Int, n: Int): Int =
    if n == 0 then acc
    else loop(acc * n, n - 1)
  loop(1, n)

def fibo(n: Int): Int = {
  if (n == 0) 0
  else if (n == 1) 1
  else fibo(n - 1) + fibo(n - 2)
}

def fiboTail(n: Int) = {
  def loop(n: Int, a: Int, b: Int): Int =
    if (n == 0) a
    else loop(n - 1, b, a + b)
  loop(n, 0, 1)
}

val v2 = (1 to 10).toVector
v2.map(x => if (x % 2 == 0) factorial(x) else fibo(x))

// 4.
def fastExp(base: Int, exp: Int): Int = {
  def loop(base: Int, exp: Int, acc: Int): Int =
    exp match
      case 0 => acc
      case x if x % 2 == 0 => loop(base * base, exp / 2, acc)
      case _ => loop(base, exp - 1, acc * base)
  loop(base, exp, 1)
}

fastExp(2, 10)
