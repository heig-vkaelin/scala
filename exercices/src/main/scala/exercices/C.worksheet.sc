// 1.
def multiplyListByVector(v: Vector[Int], l: List[Int]): Vector[List[Int]] =
  v.map(x => l.map(y => x * y))

multiplyListByVector(Vector(1, 2, 3), List(4, 5, 6))

// 2.
def max(l: List[Int]): Int =
  l.reduceLeft((x, y) => if (x > y) x else y)

max(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
max(List(1, 2, 3, 12, 5, 6, 7, 8, 9, 10))

// 3.
1.to(10).reduceLeft(_ * _)
// On obtient le même résultat que la fonction factoriel jusqu'à 10

def factorial(n: Int): Int =
  1.to(n).reduceLeft(_ * _)

factorial(10)

// 4.
def power2(n: Int): Int =
  1.to(n).map(_ => 2).reduceLeft(_ * _)

power2(3)

// 5.
def catSpace(l: Seq[String]): String =
  l.reduceLeft((x, y) => x + " " + y)

catSpace(Vector("I", "have", "a", "dream"))

// 6.
def reverse(l: List[Int]): List[Int] =
  l.foldLeft(List[Int]())((x, y) => y :: x)

reverse(List(1, 2, 3, 4, 5))
reverse(List(1, 2))
reverse(List(1))
reverse(List())

// 7.
def firstColumn(xs: List[List[Int]]): List[Int] =
  xs.map(x => x.head)

def column(xs: List[List[Int]], col: Int): List[Int] = 
  xs.map(x => x(col))

firstColumn(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))

column(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)), 0)
column(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)), 1)
column(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)), 2)

// 8.
def diagonal(xs: List[List[Int]]): List[Int] =
  xs.zipWithIndex.map(x => x._1(x._2))

diagonal(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))

// 9.
def hasZeroRow(matrix: List[List[Int]]): Boolean =
  matrix.exists(x => x.forall(_ == 0))

hasZeroRow(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
hasZeroRow(List(List(1, 2, 3), List(4, 5, 6), List(0, 0, 0)))

// 10.
def isPrime(x: Int): Boolean = 
  x > 1 && 2.to(x - 1).forall(y => x % y != 0)

isPrime(2)
isPrime(3)
isPrime(4)

// 11.
def linesLonger(lines: List[String], len: Int): List[String]= 
  lines.filter(x => x.length > len)

linesLonger(List("Hi", "World", "Scala"), 3)

// 12.
def longestLineLength(lines: List[String]): Int = 
  lines.foldLeft(0)((acc, x) => if (acc > x.length) acc else x.length)

def longestLineLengthV2(lines: List[String]): Int = 
  lines.reduceLeft((acc, x) => if (acc.length > x.length) acc else x).length

longestLineLength(List("Hi", "World-", "Scala"))
longestLineLengthV2(List("Hi", "World-", "Scala"))

// 13.
def elimEmptyLines(lines: List[String]): List[String] = 
  lines.filter(x => x.length > 0)

elimEmptyLines(List("Hello", "", "World", "Scala"))

// 14.
def longestLine(lines: List[String]): String = 
  lines.reduceLeft((acc, x) => if (acc.length > x.length) acc else x)

longestLine(List("Hello", "World", "Scala"))
longestLine(List("Hello", "World-", "Scala"))
