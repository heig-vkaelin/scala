// 1.

// 1. On souhaite créer une Map[Int, String] à partir d’un Vector[String] et d’une List[Int], ou les clés sont les
// éléments de la liste et les valeurs les éléments du vecteur (en se basant sur la collection la plus petite).
// (a) Ecrivez une première fonction récursive pour combiner les clés et les valeurs. Cette dernière prend le vecteur et la liste en paramètre et retourne une List[(Int, String)].

// a)
// def createMap(v: Vector[String], l: List[Int]): Map[Int, String] =
//   if (v.isEmpty || l.isEmpty) Nil
//   else (l.head, v.head) :: createMap(v.tail, l.tail)

// (b) Ecrivez une seconde fonction qui prend le retour de la première fonction en paramètre et qui retourne la
// Map[Int, String].

// b)

// TODO

// 2.
def init[T](xs: List[T]): List[T] = xs match
  case List() => throw new Exception("last of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)

init(List(1,2,3,4,5))
init(List(1,2))
init(List(1))

// 3.
def removeAt[T](n: Int, xs: List[T]) = 
  if (n < 0 || n >= xs.length) xs
  else xs.take(n) ++ xs.drop(n + 1)

removeAt(1, List('a', 'b', 'c', 'd'))

// 4.
def pack[T](xs: List[T]): List[List[T]] = xs match
  case Nil => Nil
  case y :: ys => 
    val (packed, rest) = xs.span(_ == y)
    packed :: pack(rest)

pack(List("a", "a", "a", "b", "c", "c", "a"))

// 5.
def encode[T](xs: List[T]): List[(T, Int)] = 
  pack(xs).map(x => (x.head, x.length))

encode(List("a", "a", "a", "b", "c", "c", "a"))

// 6.
def splitRecursive[A](n: Int, ls: List[A]): (List[A], List[A]) =
  (n, ls) match
    case (_, Nil) => (Nil, Nil)
    case (0, list) => (Nil, list)
    case (n, h :: tail) => 
      val (pre, post) = splitRecursive(n - 1, tail)
      (h :: pre, post)

splitRecursive(3, List('a', 'b', 'c', 'd', 'e'))

// 7.
def decode[T](xs: List[(T, Int)]): List[T] = 
  xs.flatMap((value, number) => List.fill(number)(value))

decode(List(("a", 3), ("b", 1), ("c", 2), ("a", 1)))

// 8.
// TODO