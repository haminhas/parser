package cfl

/**
  * Created by Hassan on 20/11/2016.
  */
import scala.language.{implicitConversions, reflectiveCalls}

object lexer {
  abstract class Rexp
  case object ZERO extends Rexp
  case object ONE extends Rexp
  case class CHAR(c: Char) extends Rexp
  case class ALT(r1: Rexp, r2: Rexp) extends Rexp
  case class SEQ1(r1: Rexp, r2: Rexp) extends Rexp
  case class STAR(r: Rexp) extends Rexp
  case class RECD(x: String, r: Rexp) extends Rexp
  case class RANGE(c: List[Char]) extends Rexp

  abstract class Val
  case object Empty extends Val
  case class Chr(c: Char) extends Val
  case class Seq(v1: Val, v2: Val) extends Val
  case class Left(v: Val) extends Val
  case class Right(v: Val) extends Val
  case class Stars(vs: List[Val]) extends Val
  case class Rec(x: String, v: Val) extends Val

  // some convenience for typing in regular expressions
  def charlist2rexp(s : List[Char]): Rexp = s match {
    case Nil => ONE
    case c::Nil => CHAR(c)
    case c::s => SEQ1(CHAR(c), charlist2rexp(s))
  }
  implicit def string2rexp(s : String) : Rexp = charlist2rexp(s.toList)

  implicit def RexpOps(r: Rexp) = new {
    def | (s: Rexp) = ALT(r, s)
    def % = STAR(r)
    def ~ (s: Rexp) = SEQ1(r, s)
  }

  implicit def stringOps(s: String) = new {
    def | (r: Rexp) = ALT(s, r)
    def | (r: String) = ALT(s, r)
    def % = STAR(s)
    def ~ (r: Rexp) = SEQ1(s, r)
    def ~ (r: String) = SEQ1(s, r)
    def $ (r: Rexp) = RECD(s, r)
  }

  // nullable function: tests whether the regular
  // expression can recognise the empty string
  def nullable (r: Rexp) : Boolean = r match {
    case ZERO => false
    case ONE => true
    case CHAR(_) => false
    case ALT(r1, r2) => nullable(r1) || nullable(r2)
    case SEQ1(r1, r2) => nullable(r1) && nullable(r2)
    case STAR(_) => true
    case RECD(_, r1) => nullable(r1)
    case RANGE (r) => false
  }

  // derivative of a regular expression w.r.t. a character
  def der (c: Char, r: Rexp) : Rexp = r match {
    case ZERO => ZERO
    case ONE => ZERO
    case CHAR(d) => if (c == d) ONE else ZERO
    case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
    case SEQ1(r1, r2) =>
      if (nullable(r1)) ALT(SEQ1(der(c, r1), r2), der(c, r2))
      else SEQ1(der(c, r1), r2)
    case STAR(r) => SEQ1(der(c, r), STAR(r))
    case RECD(_, r1) => der(c, r1)
    case RANGE(ch)=> if(ch.contains(c)) ONE else ZERO
  }

  // derivative w.r.t. a string (iterates der)
  def ders (s: List[Char], r: Rexp) : Rexp = s match {
    case Nil => r
    case c::s => ders(s, der(c, r))
  }

  // extracts a string from value
  def flatten(v: Val) : String = v match {
    case Empty => ""
    case Chr(c) => c.toString
    case Left(v) => flatten(v)
    case Right(v) => flatten(v)
    case Seq(v1, v2) => flatten(v1) + flatten(v2)
    case Stars(vs) => vs.map(flatten).mkString
    case Rec(_, v) => flatten(v)
  }

  // extracts an environment from a value
  def env(v: Val) : List[(String, String)] = v match {
    case Empty => Nil
    case Chr(c) => Nil
    case Left(v) => env(v)
    case Right(v) => env(v)
    case Seq(v1, v2) => env(v1) ::: env(v2)
    case Stars(vs) => vs.flatMap(env)
    case Rec(x, v) => (x, flatten(v))::env(v)
  }

  // injection part
  def mkeps(r: Rexp) : Val = r match {
    case ONE => Empty
    case ALT(r1, r2) =>
      if (nullable(r1)) Left(mkeps(r1)) else Right(mkeps(r2))
    case SEQ1(r1, r2) => Seq(mkeps(r1), mkeps(r2))
    case STAR(r) => Stars(Nil)
    case RECD(x, r) => Rec(x, mkeps(r))
  }


  def inj(r: Rexp, c: Char, v: Val) : Val = (r, v) match {
    case (STAR(r), Seq(v1, Stars(vs))) => Stars(inj(r, c, v1)::vs)
    case (SEQ1(r1, r2), Seq(v1, v2)) => Seq(inj(r1, c, v1), v2)
    case (SEQ1(r1, r2), Left(Seq(v1, v2))) => Seq(inj(r1, c, v1), v2)
    case (SEQ1(r1, r2), Right(v2)) => Seq(mkeps(r1), inj(r2, c, v2))
    case (ALT(r1, r2), Left(v1)) => Left(inj(r1, c, v1))
    case (ALT(r1, r2), Right(v2)) => Right(inj(r2, c, v2))
    case (CHAR(d), Empty) => Chr(c)
    case (RECD(x, r1), _) => Rec(x, inj(r1, c, v))
    case (RANGE(_), Empty) => Chr(c)

  }

  // main lexing function (produces a value)
  def lex(r: Rexp, s: List[Char]) : Val = s match {
    case Nil => if (nullable(r)) mkeps(r)
    else throw new Exception("Not matched")
    case c::cs => inj(r, c, lex(der(c, r), cs))
  }

  def lexing(r: Rexp, s: String) : Val = lex(r, s.toList)


  lexing(("ab" | "a") ~ ("b" | ONE), "ab")



  // some "rectification" functions for simplification
  def F_ID(v: Val): Val = v
  def F_RIGHT(f: Val => Val) = (v:Val) => Right(f(v))
  def F_LEFT(f: Val => Val) = (v:Val) => Left(f(v))
  def F_ALT(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
    case Right(v) => Right(f2(v))
    case Left(v) => Left(f1(v))
  }
  def F_SEQ1(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
    case Seq(v1, v2) => Seq(f1(v1), f2(v2))
  }
  def F_SEQ1_Empty1(f1: Val => Val, f2: Val => Val) =
    (v:Val) => Seq(f1(Empty), f2(v))
  def F_SEQ1_Empty2(f1: Val => Val, f2: Val => Val) =
    (v:Val) => Seq(f1(v), f2(Empty))
  def F_RECD(f: Val => Val) = (v:Val) => v match {
    case Rec(x, v) => Rec(x, f(v))
  }
  def F_ERROR(v: Val): Val = throw new Exception("error")

  // simplification of regular expressions returning also an
  // rectification function; no simplification under STAR
  def simp(r: Rexp): (Rexp, Val => Val) = r match {
    case ALT(r1, r2) => {
      val (r1s, f1s) = simp(r1)
      val (r2s, f2s) = simp(r2)
      (r1s, r2s) match {
        case (ZERO, _) => (r2s, F_RIGHT(f2s))
        case (_, ZERO) => (r1s, F_LEFT(f1s))
        case _ => if (r1s == r2s) (r1s, F_LEFT(f1s))
        else (ALT (r1s, r2s), F_ALT(f1s, f2s))
      }
    }
    case SEQ1(r1, r2) => {
      val (r1s, f1s) = simp(r1)
      val (r2s, f2s) = simp(r2)
      (r1s, r2s) match {
        case (ZERO, _) => (ZERO, F_ERROR)
        case (_, ZERO) => (ZERO, F_ERROR)
        case (ONE, _) => (r2s, F_SEQ1_Empty1(f1s, f2s))
        case (_, ONE) => (r1s, F_SEQ1_Empty2(f1s, f2s))
        case _ => (SEQ1(r1s,r2s), F_SEQ1(f1s, f2s))
      }
    }
    case RECD(x, r1) => {
      val (r1s, f1s) = simp(r1)
      (RECD(x, r1s), F_RECD(f1s))
    }
    case r => (r, F_ID)
  }

  def lex_simp(r: Rexp, s: List[Char]) : Val = s match {
    case Nil => if (nullable(r)) mkeps(r) else throw new Exception("Not matched")
    case c::cs => {
      val (r_simp, f_simp) = simp(der(c, r))
      inj(r, c, f_simp(lex_simp(r_simp, cs)))
    }
  }

  def lexing_simp(r: Rexp, s: String) : Val = lex_simp(r, s.toList)

  lexing_simp(("a" | "ab") ~ ("b" | ""), "ab")

  // Lexing Rules for a Small While Language

  def PLUS(r: Rexp) = r ~ r.%

  val SYM = RANGE(('a' to 'z').toList ++ ('A' to 'Z').toList)
  val DIGIT = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
  val ID = SYM ~ (SYM | DIGIT).%
  val NUM = PLUS(DIGIT)
  val KEYWORD : Rexp = "skip" | "while" | "do" | "if" | "then" | "else" | "read" | "write" | "true" | "false" | "write" | "read" | "for" | "upto"
  val SEMI: Rexp = ";"
  val OP: Rexp = ":=" | "==" | "-" | "+" | "*" | "!=" | "<" | ">" | "<=" | ">=" | "%" | "/" | "&&" | "||"
  val WHITESPACE = PLUS(" " | "\n" | "\t")
  val RPAREN: Rexp = ")"
  val LPAREN: Rexp = "("
  val BEGIN: Rexp = "{"
  val END: Rexp = "}"
  val STRING: Rexp = "\"" ~ (SYM | " ").% ~ "\""

  val WHILE_REGS = (("k" $ KEYWORD) |
    ("i" $ ID) |
    ("o" $ OP) |
    ("n" $ NUM) |
    ("s" $ SEMI) |
    ("str" $ STRING) |
    ("p" $ (LPAREN | RPAREN)) |
    ("b" $ (BEGIN | END)) |
    ("w" $ WHITESPACE)).%

  //   Testing
  //============

  def time[T](code: => T) = {
    val start = System.nanoTime()
    val result = code
    val end = System.nanoTime()
    println((end - start)/1.0e9)
    result
  }

  def main(args: Array[String]): Unit = {
    val r1 = ("a" | "ab") ~ ("bcd" | "c")
//    println(lexing(r1, "abcd"))

    val r2 = ("" | "a") ~ ("ab" | "b")
//    println(lexing(r2, "ab"))


    // Two Simple While Tests
    //========================
//    println("prog0 test")

    val prog0 = """read n"""
//    println(env(lexing_simp(WHILE_REGS, prog0)))

//    println("prog1 test")

    val prog1 = """read  n; write (n)"""
//    println(env(lexing_simp(WHILE_REGS, prog1)))


    // Big Test
    //==========

    val prog2 = """
    write "fib";
    read n;
    minus1 := 0;
    minus2 := 1;
    while n > 0 do {
      temp := minus2;
      minus2 := minus1 + minus2;
      minus1 := temp;
      n := n - 1
    };
    write "result";
    write minus2
                """

    val prog23 = """
    write "fib";
    read n;
    minus1 := 0;
    minus2 := 1;
    for i := 2 upto 4 do {
      temp := minus2;
      minus2 := minus1 + minus2;
      minus1 := temp;
      n := n - 1
    };
    write "result";
    write minus2
                """




//    println("Tokens")

//    println(env(lexing_simp(WHILE_REGS, prog2)))
    println(env(lexing_simp(WHILE_REGS, prog23)).filterNot{_._1 == "w"}.mkString("\n"))
  }

}

