import scala.language.implicitConversions
import scala.language.reflectiveCalls
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


  lazy val F: Parser[String, Int] =
    ("(" ~ E ~ ")") ==> { case ((x, y), z) => y } || NumParser
  lazy val T: Parser[String, Int] =
    (F ~ "*" ~ T) ==> { case ((x, y), z) => x * z } || F
  lazy val E: Parser[String, Int] =
    (T ~ "+" ~ E) ==> { case ((x, y), z) => x + z } || T


  def main(args: Array[String]): Unit = {
    println(E.parse_all("1*2+3"))
    println(E.parse_all("1+2*3"))
    println(E.parse_all("1+2+3"))
    println(E.parse_all("1+2+3"))
    println(E.parse_all("1+2*3+1"))
  }
}