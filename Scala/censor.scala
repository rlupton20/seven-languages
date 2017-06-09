import scala.io._

trait Censor {
  val rewrite: Map[String,String]
  def censor(string: String): String
}

class Censorship(rewrites: Map[String,String]) extends Censor {
  val rewrite = rewrites
  def censor(string: String): String = string.split(" +")
    .map{ str => rewrite getOrElse(str,str) }
    .foldLeft(""){ (str,word) => str ++ " " ++ word }
}

object CensorFile {
  def load(file: File): Censorship = {
    val rules: Iterator[Array[String]] = io.Source.fromFile(file).getLines()
      .map(_.split(",").map(_.trim))
    val empty: Map[String,String] = Map()
    val rulemap: Map[String,String] = rules.foldLeft(empty){ (m, rule) =>
      m + (rule(0) -> rule(1))
    }
    new Censorship(rulemap)
  }
}
