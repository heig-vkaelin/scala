// 1.
var nom = ""
var prenom = ""

// 2.
nom = "Kaelin"
prenom = "Valentin"

// 3.
val dateDeNaissance = "15-11-1997"
// dateDeNaissance = "15-11-1997" // erreur car on ne peut pas reassigner une valeur à une val

// 4.
val message = "Bonjour, je m'appelle " + prenom + " " + nom + " et je suis né le " + dateDeNaissance
println(message)

// 5.
var i = 5
while (i > 0) {
  println(message)
  i -= 1
}

// 6.
for (i <- 1 to 5) {
  println(message)
}

// 7.
for (i <- 0 until 5) {
  println(message + " " + i)
}

// 8.
println(if i == 0 then nom else prenom) 

// 9.
i match {
  case 0 => nom
  case _ => prenom
}

// 10.
var j = 1
while (j < 100) {
  if (j % 2 != 0 && j % 3 == 0 || j % 5 == 0) {
    println(j)
  }
  j += 1
}

for (j <- 1 until 100 if j % 2 != 0 && j % 3 == 0 || j % 5 == 0) println(j)

// 11.
def func(x: Int, y: Int) = if x > y then x else y
func(3, 5)

// 12.
def hello(x: String) = x match {
  case "Hello" => "World"
  case "World" => "Hello"
  case _ => "Goodbye"
}
hello("Hello")
hello("World")
hello("Test")

// 13.
for
  i <- 0 to 2
  j <- 0 to 3
  k <- 1 to 4
do
  println((i * 16 + j * 4 + k).toString() + ". Hello World !")
// Hello World ! est affiché 48 fois

// 14.
val myString = "Teste"
myString match {
  case x if myString.length() % 2 != 0 => myString.length()
  case "Hello World!" => println(myString)
  case _ => println("Goodbye")
}

// 15.
var compteur = 0
while (compteur < 10) {
  compteur match {
    case x if compteur % 2 == 0 => compteur += 3
    case _ => compteur += 1
  }
  println(compteur)
}
// La boucle est exécutée 5 fois, le compteur vaut 3, 4, 7, 8, 11

// 16.
def recursive(x: Int): Int = {
  println(x)
  if (x > 100) return x
  if (x % 7 == 0) return recursive(x + 8)
  if (x % 2 != 0) return recursive(x + 12)
  return recursive(x + 1)
}
val x = 0
recursive(x)
// 0
// 8
// 9
// 21
// 29
// 41
// 53
// 65
// 77
// 85
// 97
// 109
// retourne 109, la valeur de x reste inchangée, la fonction recursive est exécutée 12 fois
// le type de retour de la fonction (Int) est obligatoire (sinon erreur de compilation)

// 17.
def calculate(x : Int, y : Int, z : Int) : Int = {
  z match
      case z if z % 2 == 0 => x*x + y*y
      case z if z % 2 == 1 => (x+y) * (x+y)
      case 0 => x*x + y*y + (x+y) * (x+y)
}
println(calculate(2, 3, 1))
println(calculate(2, 3, 0))
println(calculate(2, 3, 2))
// Cas où z vaut 0 est faux car devrait être placé avant le cas où z est pair

// 18.
def myFuncBool(x: Int, y: Int): Int = {
  if (x == y) return x
  if (x > y) {
    if (x % 2 == 0) return 2 * x + 3 * y
    if (y % 2 != 0 && x % 2 == 0) return 4 * x - 7 * y
    if (x % 3 == 0 && y % 4 == 0) return x * x + math.pow(y, 3).toInt
  }
  if (x < y) {
    if (x > 4) return 2 * x
    return y
  }
  return 9
}

def myFuncMatch(x : Int, y : Int) : Int = {
  (x, y) match
    case (x, y) if x == y => x
    case (x, y) if x < y && x > 4 => 2 * x
    case (x, y) if x < y => y
    case (x, y) if x % 2 == 0 => 2 * x + 3 * y
    case (x, y) if y % 2 != 0 && x % 2 == 0 => 4 * x - 7 * y
    case (x, y) if x % 3 == 0 && y % 4 == 0 => x * x + math.pow(y, 3).toInt
    case _ => 9
}

// 19.
def fibo(n: Int): Int = {
  if (n == 0) return 0
  if (n == 1) return 1
  return fibo(n - 1) + fibo(n - 2)
}

