def factorial(n: Int): BigInt =
  def loop(acc: BigInt, n: Int): BigInt =
    if n == 0 then acc
    else loop(acc * n, n - 1)
  loop(1, n)

factorial(4)

// def fibo(n: Int): Int = {
//   if (n == 0) return 0
//   if (n == 1) return 1
//   return fibo(n - 1) + fibo(n - 2)
// }

def fibo(n: Int): Int = {
  def loop(n: Int, a: Int, b: Int): Int =
    if (n == 0) a
    else loop(n - 1, b, a + b)
  loop(n, 0, 1)
}

fibo(10)


class Rational(x: Int, y: Int):
  require(y != 0, "denominator must be non-zero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int =
    if b == 0 then a else gcd(b, a % b)

  private val g = gcd(x, y).abs
  
  val numer = x / g
  val denom = y / g

  def add(r: Rational) = Rational(numer * r.denom + r.numer * denom, denom * r.denom)
  def mul(r: Rational) = Rational(numer * r.numer, denom * r.denom)
  def neg = Rational(-numer, denom)
  def sub (r: Rational) = add(r.neg)
  def less (r: Rational) = numer * r.denom < r.numer * denom
  def max (r: Rational) = if less(r) then r else this

  override def toString = s"$numer/$denom"

end Rational // optionnel pour la lisibilité

extension (r: Rational)
  infix def min(s: Rational) = if s.less(r) then s else r
  def abs: Rational = Rational(r.numer.abs, r.denom)
  def + (y: Rational) = x.add(y)
  def + (y: Int) = x.add(Rational(y))
  def * (y: Rational) = x.mul(y)

extension (i: Int)
  def + (y: Rational) = x.add(Rational(i))

val x = Rational(1, 2)
val y = Rational(5, 7)
val z = Rational(3, 2)
x.numer
x.denom

x.add(y)
x + y
x.mul(y)
x * y
x.neg
val xx = x.sub(y).sub(z)
x + y * z

val entier = Rational(2)

val r1 = Rational(1, 3)
val r2 = Rational(5, 7)
r1.less(r2)
r2.max(r1)
r2.min(r1)
r2 min r1

val r3 = Rational(1, 2)
val r4 = r3 + 1
val r5 = 1 + r3
