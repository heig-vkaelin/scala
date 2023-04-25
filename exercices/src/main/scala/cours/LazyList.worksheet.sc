// 1. Ecrire une fonction qui prend un LazyList en paramètre et le transforme en une List:
def toList[A](s: LazyList[A]): List[A] =
  s match {
    case LazyList() => List()
    case h #:: t    => h :: toList(t)
  }

toList(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
toList(LazyList(1, 2))
toList(LazyList(1))
toList(LazyList())

// 2. Ecrire une fonction qui prend un LazyList en paramètre et retourne le LazyList
// contenant seulement les n premiers éléments.
def take[A](s: LazyList[A], n: Int): LazyList[A] =
  s match {
    case LazyList() => LazyList()
    case h #:: t    => if (n > 0) h #:: take(t, n - 1) else LazyList()
  }

toList(take(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 5))
toList(take(LazyList(1, 2, 3, 4, 5), 0))
toList(take(LazyList(1, 2, 3, 4, 5), 10))
toList(take(LazyList(), 10))
toList(take(LazyList(1,2,3), -10))
