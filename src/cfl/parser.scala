package cfl

import scala.language.{implicitConversions, reflectiveCalls}
object parser {
  abstract class Parser[I <% Seq[_], T] {
    def parse(ts: I): Set[(T, I)]

    def parse_all(ts: I): Set[T] =
      for ((head, tail) <- parse(ts); if (tail.isEmpty)) yield head
  }

  class SeqParser[I <% Seq[_], T, S](p: => Parser[I, T], q: => Parser[I, S]) extends Parser[I, (T, S)] {
    def parse(sb: I) =
      for ((head1, tail1) <- p.parse(sb);
           (head2, tail2) <- q.parse(tail1)) yield ((head1, head2), tail2)
  }

  class AltParser[I <% Seq[_], T](p: => Parser[I, T], q: => Parser[I, T]) extends Parser[I, T] {
    def parse(sb: I) = p.parse(sb) ++ q.parse(sb)
  }

  class FunParser[I <% Seq[_], T, S](p: => Parser[I, T], f: T => S) extends Parser[I, S] {
    def parse(sb: I) =
      for ((head, tail) <- p.parse(sb)) yield (f(head), tail)
  }

  case class StringParser(s: String) extends Parser[String, String] {
    def parse(sb: String) = {
      val (prefix, suffix) = sb.splitAt(s.length)
      if (prefix == s) Set((prefix, suffix)) else Set()
    }
  }

  case object NumParser extends Parser[String, Int] {
    val reg = "[0-9]+".r

    def parse(sb: String) = reg.findPrefixOf(sb) match {
      case None => Set()
      case Some(s) => {
        val (head, tail) = sb.splitAt(s.length)
        Set((head.toInt, tail))
      }
    }
  }


  implicit def string2parser(s: String) = StringParser(s)

  implicit def ParserOps[I <% Seq[_], T](p: Parser[I, T]) = new {
    def ||(q: => Parser[I, T]) = new AltParser[I, T](p, q)

    def ==>[S](f: => T => S) = new FunParser[I, T, S](p, f)

    def ~[S](q: => Parser[I, S]) = new SeqParser[I, T, S](p, q)
  }

  implicit def StringOps(s: String) = new {
    def ||(q: => Parser[String, String]) = new AltParser[String, String](s, q)

    def ||(r: String) = new AltParser[String, String](s, r)

    def ==>[S](f: => String => S) = new FunParser[String, String, S](s, f)

    def ~[S](q: => Parser[String, S]) =
      new SeqParser[String, String, S](s, q)

    def ~(r: String) =
      new SeqParser[String, String, String](s, r)
  }

  lazy val E: Parser[String, Int] =
    (T ~ "+" ~ E) ==> { case ((x, y), z) => x + z } || T
  lazy val T: Parser[String, Int] =
    (F ~ "*" ~ T) ==> { case ((x, y), z) => x * z } || F
  lazy val F: Parser[String, Int] =
    ("(" ~ E ~ ")") ==> { case ((x, y), z) => y } || NumParser


  // the abstract syntax trees for the WHILE language
  abstract class Stmt
  abstract class AExp
  abstract class BExp

  type Block = List[Stmt]

  case object Skip extends Stmt
  case class If(a: BExp, bl1: Block, bl2: Block) extends Stmt
  case class While(b: BExp, bl: Block) extends Stmt
  case class Assign(s: String, a: AExp) extends Stmt

  case class Var(s: String) extends AExp
  case class Num(i: Int) extends AExp
  case class Aop(o: String, a1: AExp, a2: AExp) extends AExp

  case object True extends BExp
  case object False extends BExp
  case class Bop(o: String, a1: AExp, a2: AExp) extends BExp


  case object IdParser extends Parser[String, String] {
    val reg = "[a-z][a-z,0-9]*".r
    def parse(sb: String) = reg.findPrefixOf(sb) match {
      case None => Set()
      case Some(s) => Set(sb.splitAt(s.length))
    }
  }

  lazy val AExp: Parser[String, AExp] =
    (Te ~ "+" ~ AExp) ==> { case ((x, y), z) => Aop("+", x, z):AExp } ||
      (Te ~ "-" ~ AExp) ==> { case ((x, y), z) => Aop("-", x, z):AExp } || Te
  lazy val Te: Parser[String, AExp] =
    (Fa ~ "*" ~ Te) ==> { case ((x, y), z) => Aop("*", x, z):AExp } || Fa
  lazy val Fa: Parser[String, AExp] =
    ("(" ~ AExp ~ ")") ==> { case ((x, y), z) => y } ||
      IdParser ==> Var ||
      NumParser ==> Num

  // boolean expressions
  lazy val BExp: Parser[String, BExp] =
  (AExp ~ "=" ~ AExp) ==> { case ((x, y), z) => Bop("=", x, z): BExp } ||
    (AExp ~ "!=" ~ AExp) ==> { case ((x, y), z) => Bop("!=", x, z): BExp } ||
    (AExp ~ "<" ~ AExp) ==> { case ((x, y), z) => Bop("<", x, z): BExp } ||
    (AExp ~ ">" ~ AExp) ==> { case ((x, y), z) => Bop(">", x, z): BExp } ||
    ("true" ==> ((_) => True: BExp)) ||
    ("false" ==> ((_) => False: BExp)) ||
    ("(" ~ BExp ~ ")") ==> { case ((x, y), z) => y}

  lazy val Stmt: Parser[String, Stmt] =
    ("skip" ==> ((_) => Skip: Stmt)) ||
      (IdParser ~ ":=" ~ AExp) ==> { case ((x, y), z) => Assign(x, z): Stmt } ||
      ("if" ~ BExp ~ "then" ~ Block ~ "else" ~ Block) ==>
        { case (((((x,y),z),u),v),w) => If(y, u, w): Stmt } ||
      ("while" ~ BExp ~ "do" ~ Block) ==> { case (((x, y), z), w) => While(y, w) }

  lazy val Stmts: Parser[String, Block] =
    (Stmt ~ ";" ~ Stmts) ==> { case ((x, y), z) => x :: z : Block } ||
      (Stmt ==> ((s) => List(s) : Block))

  lazy val Block: Parser[String, Block] =
    ("{" ~ Stmts ~ "}") ==> { case ((x, y), z) => y} ||
      (Stmt ==> ((s) => List(s)))


  // an interpreter for the WHILE language
  type Env = Map[String, Int]

  def eval_aexp(a: AExp, env : Env) : Int = a match {
    case Num(i) => i
    case Var(s) => env(s)
    case Aop("+", a1, a2) => eval_aexp(a1, env) + eval_aexp(a2, env)
    case Aop("-", a1, a2) => eval_aexp(a1, env) - eval_aexp(a2, env)
    case Aop("*", a1, a2) => eval_aexp(a1, env) * eval_aexp(a2, env)
  }

  def eval_bexp(b: BExp, env: Env) : Boolean = b match {
    case True => true
    case False => false
    case Bop("=", a1, a2) => eval_aexp(a1, env) == eval_aexp(a2, env)
    case Bop("!=", a1, a2) => !(eval_aexp(a1, env) == eval_aexp(a2, env))
    case Bop(">", a1, a2) => eval_aexp(a1, env) > eval_aexp(a2, env)
    case Bop("<", a1, a2) => eval_aexp(a1, env) < eval_aexp(a2, env)
  }

  def eval_stmt(s: Stmt, env: Env) : Env = s match {
    case Skip => env
    case Assign(x, a) => env + (x -> eval_aexp(a, env))
    case If(b, bl1, bl2) => if (eval_bexp(b, env)) eval_bl(bl1, env) else eval_bl(bl2, env)
    case While(b, bl) =>
      if (eval_bexp(b, env)) eval_stmt(While(b, bl), eval_bl(bl, env))
      else env
  }

  def eval_bl(bl: Block, env: Env) : Env = bl match {
    case Nil => env
    case s::bl => eval_bl(bl, eval_stmt(s, env))
  }

  def eval(bl: Block) : Env = eval_bl(bl, Map())

  def main(args: Array[String]): Unit = {
    println(E.parse_all("1+2*3"))
    println(E.parse_all("1+2+3"))
    println(E.parse_all("1+2+3"))
    println(E.parse_all("1+2*3+1"))

    println(Block.parse_all("x2:=5"))
    println(Block.parse_all("{x:=5;y:=8}"))
    println(Block.parse_all("if(false)then{x:=5}else{x:=10}"))

    val fib = """{n:=10;minus1:=0;minus2:=1;temp:=0;while(n>0)do{temp:=minus2;minus2:=minus1+minus2;minus1:=temp;n:=n-1};result:=minus2}"""
    val tokens = """List((k,write), (str,\"fib\"), (s,;), (k,read), (i,n), (s,;), (i,minus1), (o,:=), (n,0), (s,;), (i,minus2), (o,:=), (n,1), (s,;), (k,while), (i,n), (o,>), (n,0), (k,do), (b,{), (i,temp), (o,:=), (i,minus2), (s,;), (i,minus2), (o,:=), (i,minus1), (o,+), (i,minus2), (s,;), (i,minus1), (o,:=), (i,temp), (s,;), (i,n), (o,:=), (i,n), (o,-), (n,1), (b,}), (s,;), (k,write), (str,\"result\"), (s,;), (k,write), (i,minus2))"""
    Block.parse_all(fib)

    println(eval(Block.parse_all(fib).head)("result"))
//    println(eval(Block.parse_all(tokens).head)("result"))


  }
}