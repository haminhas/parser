package cfl

import scala.language.implicitConversions
import scala.language.reflectiveCalls

object parser {

  abstract class Parser[I <% Seq[_], T] {
    def parse(ts: I): Set[(T, I)]

    def parse_all(ts: I) : Set[T] =
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

  case class ParserToken(s: (String, String)) extends Parser[List[(String, String)], String] {
    def parse(sb: List[(String,String)]) = {
      if(sb.nonEmpty && sb.head == s) Set((sb.head._2, sb.tail)) else Set()
    }
  }

  case object ParserNum extends Parser[List[(String, String)], Int] {
    def parse(sb: List[(String, String)]) = {
      if(sb.head._1 == "n") Set((sb.head._2.toInt, sb.tail)) else Set()
    }
  }


  implicit def ParserOps[I<% Seq[_], T](p: Parser[I, T]) = new {
    def || (q : => Parser[I, T]) = new AltParser[I, T](p, q)
    def ==>[S] (f: => T => S) = new FunParser[I, T, S](p, f)
    def ~[S] (q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
  }

  val OPPlus = ParserToken(("o","+"))
  val OPTimes = ParserToken(("o","*"))
  val OPMinus = ParserToken(("o","-"))
  val BracketLeft = ParserToken(("p","("))
  val BracketRight = ParserToken(("p",")"))

  lazy val E: Parser[List[(String,String)], Int] =
    (T ~ OPPlus  ~ E) ==> { case ((x, y), z) => x + z} || T
  lazy val T: Parser[List[(String,String)], Int] =
    (F ~ OPTimes ~ T) ==> { case ((x, y), z) => x * z} || F
  lazy val F: Parser[List[(String,String)], Int] =
    (BracketLeft ~ E ~ BracketRight) ==> { case ((x, y), z) => y} || ParserNum

  // no left-recursion allowed
  lazy val EL: Parser[List[(String,String)], Int] =
  (EL ~ OPPlus ~ EL) ==> { case ((x, y), z) => x + z} ||
    (EL ~ OPTimes ~ EL) ==> { case ((x, y), z) => x * z} ||
    (BracketLeft ~ EL ~ BracketRight) ==> { case ((x, y), z) => y} ||
    ParserNum

  // the abstract syntax trees for the WHILE language
  abstract class Stmt
  abstract class AExp
  abstract class BExp

  type Block = List[Stmt]

  case object Skip extends Stmt
  case class If(a: BExp, bl1: Block, bl2: Block) extends Stmt
  case class While(b: BExp, bl: Block) extends Stmt
  case class Assign(s: String, a: AExp) extends Stmt
  case class Write(a: AExp) extends Stmt
  case class Read(a: AExp) extends Stmt

  case class Var(s: String) extends AExp
  case class Str(s: String) extends AExp
  case class Num(i: Int) extends AExp
  case class Aop(o: String, a1: AExp, a2: AExp) extends AExp

  case object True extends BExp
  case object False extends BExp
  case class Bop(o: String, a1: AExp, a2: AExp) extends BExp
  case class Bop2(o: String, b1: BExp, b2: BExp) extends BExp


  case object ParserID extends Parser[List[(String, String)], String] {
    def parse(sb: List[(String, String)]) = {
      if(sb.head._1 == "i") Set((sb.head._2, sb.tail)) else Set()
    }
  }

  case object ParserString extends Parser[List[(String, String)], String] {
    def parse(sb: List[(String, String)]) = {
      if(sb.head._1 == "str") Set((sb.head._2, sb.tail)) else Set()
    }
  }

  lazy val AExp: Parser[List[(String,String)], AExp] =
    (Te ~ OPPlus ~ AExp) ==> { case ((x, y), z) => Aop("+", x, z):AExp } ||
      (Te ~ OPMinus ~ AExp) ==> { case ((x, y), z) => Aop("-", x, z):AExp } || Te
  lazy val Te: Parser[List[(String,String)], AExp] =
    (Fa ~ OPTimes ~ Te) ==> { case ((x, y), z) => Aop("*", x, z):AExp } ||
      (Fa ~ ParserToken(("o","/")) ~ Te) ==> { case ((x, y), z) => Aop("/", x, z):AExp } ||
        (Fa ~ ParserToken(("o","%")) ~ Te) ==> { case ((x, y), z) => Aop("%", x, z):AExp } || Fa
  lazy val Fa: Parser[List[(String,String)], AExp] =
    (BracketLeft ~ AExp ~ BracketRight) ==> { case ((x, y), z) => y } ||
      ParserID ==> Var ||
      ParserNum ==> Num ||
      ParserString ==> Str

  lazy val BExp: Parser[List[(String,String)], BExp] =
    (AExp ~ ParserToken(("o","==")) ~ AExp) ==> { case ((x, y), z) => Bop("==", x, z): BExp } ||
      (AExp ~ ParserToken(("o","!=")) ~ AExp) ==> { case ((x, y), z) => Bop("!=", x, z): BExp } ||
      (AExp ~ ParserToken(("o","<")) ~ AExp) ==> { case ((x, y), z) => Bop("<", x, z): BExp } ||
      (AExp ~ ParserToken(("o",">")) ~ AExp) ==> { case ((x, y), z) => Bop(">", x, z): BExp } ||
      (AExp ~ ParserToken(("o","<=")) ~ AExp) ==> { case ((x, y), z) => Bop("<=", x, z): BExp } ||
      (AExp ~ ParserToken(("o",">=")) ~ AExp) ==> { case ((x, y), z) => Bop(">=", x, z): BExp } ||
      (ParserToken(("k","true")) ==> ((_) => True: BExp)) ||
      (ParserToken(("k","false")) ==> ((_) => False: BExp)) ||
      (BracketLeft ~ BExp ~ BracketRight) ==> { case ((x, y), z) => y}

  lazy val Stmt: Parser[List[(String,String)], Stmt] =
    (ParserToken(("k","skip")) ==> ((_) => Skip: Stmt)) ||
      (ParserID ~ ParserToken(("o",":=")) ~ AExp) ==> { case ((x, y), z) => Assign(x, z): Stmt } ||
      (ParserToken(("k","if")) ~ BExp ~ ParserToken(("k","then")) ~ Block ~ ParserToken(("k","else")) ~ Block) ==>
        { case (((((x,y),z),u),v),w) => If(y, u, w): Stmt } ||
      (ParserToken(("k","while")) ~ BExp ~ ParserToken(("k","do")) ~ Block) ==> { case (((x, y), z), w) => While(y, w) } ||
      ParserToken(("k","write")) ~ AExp ==> { case (x, y) => Write(y) } ||
      ParserToken(("k","read")) ~ AExp ==> { case (x, y) => Read(y) }


  lazy val Stmts: Parser[List[(String,String)], Block] =
    (Stmt ~ ParserToken(("s",";")) ~ Stmts) ==> { case ((x, y), z) => x :: z : Block } ||
      (Stmt ==> ((s) => List(s) : Block))

  lazy val Block: Parser[List[(String,String)], Block] =
    (ParserToken(("b","{")) ~ Stmts ~ ParserToken(("b","}"))) ==> { case ((x, y), z) => y} ||
      (Stmt ==> ((s) => List(s)))

  type Env = Map[String, Int]

  def eval_aexp(a: AExp, env : Env) : Int = a match {
    case Num(i) => i
    case Var(s) => env(s)
    case Str(s) => env(s)
    case Aop("+", a1, a2) => eval_aexp(a1, env) + eval_aexp(a2, env)
    case Aop("-", a1, a2) => eval_aexp(a1, env) - eval_aexp(a2, env)
    case Aop("*", a1, a2) => eval_aexp(a1, env) * eval_aexp(a2, env)
    case Aop("%", a1, a2) => eval_aexp(a1, env) % eval_aexp(a2, env)
    case Aop("/", a1, a2) => eval_aexp(a1, env) / eval_aexp(a2, env)
  }

  def eval_bexp(b: BExp, env: Env) : Boolean = b match {
    case True => true
    case False => false
    case Bop("==", a1, a2) => eval_aexp(a1, env) == eval_aexp(a2, env)
    case Bop("!=", a1, a2) => !(eval_aexp(a1, env) == eval_aexp(a2, env))
    case Bop(">", a1, a2) => eval_aexp(a1, env) > eval_aexp(a2, env)
    case Bop("<", a1, a2) => eval_aexp(a1, env) < eval_aexp(a2, env)
    case Bop(">=", a1, a2) => eval_aexp(a1, env) >= eval_aexp(a2, env)
    case Bop("<=", a1, a2) => eval_aexp(a1, env) <= eval_aexp(a2, env)
  }

  def eval_print(a: AExp, env : Env) : String = a match {
    case Str(s) => s
    case Var(s) => s
  }

  def eval_stmt(s: Stmt, env: Env) : Env = s match {
    case Skip => env
    case Write(x) => {
      if (x.isInstanceOf[Str]){
        println(eval_print(x,env))
      } else {
        println(eval_aexp(x,env))
      }
      env
    }
    case Read(x) => {
      val a = scala.io.StdIn.readInt()
      env + (eval_print(x, env) -> a)
    }
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

  def time[T](code: => T) = {
    val start = System.nanoTime()
    val result = code
    val end = System.nanoTime()
    println((end - start)/1.0e9)
    result
  }


  def main(args: Array[String]) {
    val q2 = """if a < b then skip else a := a * b + 1"""
    val fib = """{n:=10;minus1:=0;minus2:=1;temp:=0;while(n>0)do{temp:=minus2;minus2:=minus1+minus2;minus1:=temp;n:=n-1};result:=minus2}"""
    val fig3 =
      """{
        write "Fib";
        start := 840;
        x := start;
        y := start;
        z := start; while 0 < x do {
        while 0 < y do {
        while 0 < z do { z := z - 1 }; z := start;
        y := y - 1
        };
        y := start; x := x - 1
        }
        }"""
    val fig2=
      """{
       write "Fib";
         read n;
         minus1 := 0;
         minus2 := 1;
         while n>0 do {
         temp := minus2;
         minus2 := minus1 + minus2;
         minus1 := temp;
         n := n - 1
         };
         write "Result";
         write minus2
        }"""
    val tokens = lexer.env(lexer.lexing_simp(lexer.WHILE_REGS, fig2)).filterNot{_._1 == "w"}
    println(tokens)
    println(Block.parse_all(tokens))
    println(eval(Block.parse_all(tokens).head))

   // time needed testing
   // println(time(eval(Block.parse_all(lexer.env(lexer.lexing_simp(lexer.WHILE_REGS, fig3)).filterNot{_._1 == "w"}).head)))
  }
}





