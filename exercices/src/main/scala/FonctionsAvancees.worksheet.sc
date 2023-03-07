def penultimate[T](l: List[T]): T = l match {
  case h :: _ :: Nil => h
  case _ :: tail => penultimate(tail)
  case _ => throw new NoSuchElementException
}

penultimate(List(1, 1, 2, 3, 5, 8))

def isPalindrome[T](l: List[T]): Boolean = l match {
  case Nil => true
  case h :: Nil => true
  case h :: tail => h == tail.last && isPalindrome(tail.init)
}

isPalindrome(List(1, 2, 3, 2, 1))
isPalindrome(List(1, 2, 3, 3, 1))


def removeAt[T](n: Int, l: List[T]): List[T] = l match {
  case Nil => Nil
  case h :: tail if n == 0 => tail
  case h :: tail => h :: removeAt(n - 1, tail)
}

removeAt(0, List('a', 'b', 'c', 'd'))
removeAt(1, List('a', 'b', 'c', 'd'))
removeAt(2, List('a', 'b', 'c', 'd'))
removeAt(3, List('a', 'b', 'c', 'd'))

def sum(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 0
  else f(a) + sum(f)(a + 1, b)
}

def sumInts = sum(x => x)
def sumCubes = sum(x => x * x * x)

sumInts(1, 4)