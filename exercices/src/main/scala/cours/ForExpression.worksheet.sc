def mapFun[T, U](xs: List[T], f: T => U): List[U] = 
  for x <- xs yield f(x)

def flatMap[T, U](xs: List[T], f: T => Iterable[U]): List[U] = 
  for x <- xs; y <- f(x) yield y

def filter[T](xs: List[T], p: T => Boolean): List[T] = 
  for x <- xs if p(x) yield x

filter(List(1,2,3,4,5,6,7,8,9,10), _ % 2 == 0)
filter(List(1,2,3,4,5,6,7,8,9,10), _ % 2 == 1)
filter(List(1,2,3,4,5,6,7,8,9,10), _ > 5)
