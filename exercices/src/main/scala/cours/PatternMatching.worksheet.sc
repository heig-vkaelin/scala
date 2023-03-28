trait Expr:
  def eval: Int = this match
    case Sum(e1, e2) => e1.eval + e2.eval
    case Prod(e1, e2) => e1.eval * e2.eval
    case Number(n) => n
  def show: String = this match
      case Number(n) => n.toString
      case Sum(e1, e2) => e1.show + " + " + e2.show
      case Prod(e1, e2) => 
        def showParentheses(e: Expr) = e match
          case Number(_) => e.show
          case _ => "(" + e.show + ")"
        showParentheses(e1) + " * " + showParentheses(e2)
  def simplify: Expr = this match
        case Sum(Prod(e1,e2), Prod(e3,e4)) if e1 == e3 => Prod(e1, Sum(e2, e4)).simplify
        case Sum(Prod(e1,e2), Prod(e3,e4)) if e1 == e4 => Prod(e1, Sum(e2, e3)).simplify
        case Sum(Prod(e1,e2), Prod(e3,e4)) if e2 == e3 => Prod(e2, Sum(e1, e4)).simplify
        case Sum(Prod(e1,e2), Prod(e3,e4)) if e2 == e4 => Prod(e2, Sum(e1, e3)).simplify 
        case Sum(e1, e2) => Sum(e1.simplify, e2.simplify)
        case Prod(e1, e2) => Prod(e1.simplify, e2.simplify)
        case _ => this
  def simplify_v2: Expr = this match
    case Sum(p1@Prod(e1,e2), p2@Prod(e3,e4)) =>
      if e1 == e3 then Prod(e1, Sum(e2, e4)).simplify
      else if e1 == e4 then Prod(e1, Sum(e2, e3)).simplify
      else if e2 == e3 then Prod(e2, Sum(e1, e4)).simplify
      else if e2 == e4 then Prod(e2, Sum(e1, e3)).simplify
      else Sum(p1.simplify, p2.simplify)
    case Sum(e1, e2) => Sum(e1.simplify, e2.simplify)
    case Prod(e1, e2) => Prod(e1.simplify, e2.simplify)
    case _ => this

case class Number(n: Int) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr

// 1. Ecrire une fonction show qui utilise le pattern matching pour retourner une
// représentation en chaine de caractères d'une expression donnée.
(Prod(Sum(Number(1), Number(2)), Number(3))).show

// 2. Ecrire une fonction simlify qui utilise le pattern matching pour simplifier une
// expression en utilisant la règle suivante:
// a * b + a * c → a * (b + c)
Sum(Prod(Number(4), Number(2)), Prod(Number(4), Number(3))).simplify.show
Sum(Prod(Number(2), Number(4)), Prod(Number(4), Number(3))).simplify.show
Sum(Prod(Number(2), Number(4)), Prod(Number(3), Number(4))).simplify.show

Sum(Prod(Number(4), Number(2)), Prod(Number(4), Number(3))).simplify_v2.show
Sum(Prod(Number(2), Number(4)), Prod(Number(4), Number(3))).simplify_v2.show
Sum(Prod(Number(2), Number(4)), Prod(Number(3), Number(4))).simplify_v2.show