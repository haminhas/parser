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
      if(sb.nonEmpty && sb.head == s) Set((sb.head._2,sb.tail)) else Set()
    }
  }

  case object ParserNum extends Parser[List[(String, String)], Int] {
    def parse(sb: List[(String, String)]) = {
      if(sb.head._1 == "n") Set((sb.head._2.toInt,sb.tail)) else Set()
    }
  }

  implicit def ParserOps[I<% Seq[_], T](p: Parser[I, T]) = new {
    def || (q : => Parser[I, T]) = new AltParser[I, T](p, q)
    def ==>[S] (f: => T => S) = new FunParser[I, T, S](p, f)
    def ~[S] (q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
  }

//  val plusOP = ParserToken(("o","+"))
//  val multiOP = ParserToken(("o","*"))
//  val leftBracket = ParserToken(("p","("))
//  val rightBracket = ParserToken(("p",")"))

  lazy val E: Parser[List[(String,String)], Int] =
    (T ~ ParserToken(("o","+"))  ~ E) ==> { case ((x, y), z) => x + z} || T
  lazy val T: Parser[List[(String,String)], Int] =
    (F ~ ParserToken(("o","*")) ~ T) ==> { case ((x, y), z) => x * z} || F
  lazy val F: Parser[List[(String,String)], Int] =
    (ParserToken(("p","(")) ~ E ~ ParserToken(("p",")"))) ==> { case ((x, y), z) => y} || ParserNum

  //  lazy val E: Parser[List[(String,String)], Int] =
  //    (T ~ "+"  ~ E) ==> { case ((x, y), z) => x + z} || T
  //  lazy val T: Parser[String, Int] =
  //    (F ~ "*" ~ T) ==> { case ((x, y), z) => x * z} || F
  //  lazy val F: Parser[String, Int] =
  //    ("(" ~ E ~ ")") ==> { case ((x, y), z) => y} || NumParser

  //  println(E.parse_all("1*2+3"))
  //  println(E.parse_all("1+2*3"))
  //  println(E.parse_all("1+2+3"))
  //  println(E.parse_all("1+2+3"))
  //  println(E.parse_all("1+2*3+1"))

  def main(args: Array[String]) {
    val testProgram = """60+24*2"""
    val tokens = lexer.env(lexer.lexing_simp(lexer.WHILE_REGS, testProgram)).filterNot{_._1 == "WHITESPACE"}
    println(tokens)
    println(E.parse_all(tokens))
  }
}






