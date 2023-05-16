// 1.
def toList[A](s:LazyList[A]): List[A] = s match {
  case LazyList() => List()
  case x #:: xs => x :: toList(xs)
}

toList(LazyList(1, 2, 3, 4, 5))
toList(LazyList())

// 2.
def take[A](s: LazyList[A], n: Int) : LazyList[A] = 
  if (n <= 0) LazyList()
  else s match {
    case LazyList() => LazyList()
    case x #:: xs => x #:: take(xs, n-1)
  }

take(LazyList(1, 2, 3, 4, 5), 3).toList
take(LazyList(1, 2, 3, 4, 5), 20).toList
take(LazyList(1, 2, 3, 4, 5), 1).toList
take(LazyList(1, 2, 3, 4, 5), 0).toList
take(LazyList(1, 2, 3, 4, 5), -2).toList

// 3.
def forAll[A](l: LazyList[A])(p: A => Boolean) : Boolean = l match {
    case LazyList() => true
    case x #:: xs => if (p(x)) forAll(xs)(p) else false
  }

forAll(LazyList(1, 2, 3, 4, 5))(_ < 10)
forAll(LazyList(1, 2, 3, 4, 5))(_ < 3)

// 4.
def firstNFibs(n:Int) : List[Int] = 
  lazy val fibs: LazyList[Int] = 0 #:: 1 #:: fibs.zip(fibs.tail).map { (a, b) => a + b }
  fibs.take(n).toList

firstNFibs(10)

// 5. a)
def createAbLazyList: LazyList[String] = {
  def loop(s: LazyList[String]): LazyList[String] = s match {
    case head #:: tail =>
      head #:: loop(tail ++ LazyList(head + "a", head + "b"))
  }
  "" #:: loop(LazyList("a", "b"))
}

createAbLazyList.take(10).toList

// 5. b)
def nPalindroms(n: Int, s: LazyList[String]): List[String] = {
  def isPalindrome(s: String): Boolean = s == s.reverse
  s.filter(isPalindrome).take(n).toList
}

nPalindroms(10, createAbLazyList)

// 6.
def syracusN(n: Int): LazyList[Int] = 
  n #:: syracusN(if (n % 2 == 0) n / 2 else n * 3 + 1)

syracusN(14) take 5 foreach println
