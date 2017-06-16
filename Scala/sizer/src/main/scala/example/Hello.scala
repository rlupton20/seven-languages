package example

import scala.io._
import akka.actor._


object Hello extends Greeting with App {
  import Sizer._
  import java.util.UUID.randomUUID

  val system: ActorSystem = ActorSystem("sizer")

  val urls = List( "http://www.amazon.com",
                   "http://www.twitter.com",
                   "http://www.google.com",
                   "http://www.ebay.co.uk",
                   "http://www.cnn.com" )

  try {
    // Create the aggregating actor
    val aggregator = system.actorOf(Aggregator.props, "aggregator")
    // Create worker actors, one for each link
    val workers = urls.foldLeft(List():List[ActorRef]) { (actors, url) =>
      actors ++ List(system.actorOf(Sizer.props(aggregator),
        "worker" + randomUUID.toString))
    }

    // Send work to the actors
    // First size each page
    workers.zip(urls).map{ case (worker,url) => worker ! SizeOf(url) }
    // Then count the links
    workers.zip(urls).map{ case (worker,url) => worker ! CountLinks(url) }
    // Then size the page, and all pages linked from the page, recursively
    workers.zip(urls).map{ case (worker,url) => worker ! RecursiveSize(url) }

    println(">>> Press ENTER to quit <<<")
    StdIn.readLine()
  } finally {
    system.terminate()
  }
}


// Aggreagtor collects results from actors and logs them
object Aggregator {
  case class SizeOf(url: String, size: Int)
  case class NumberOfLinks(url: String, links: Int)
  case class LinkSteps(links: List[String])

  def props(): Props = Props(new Aggregator())
}

class Aggregator extends Actor with ActorLogging {
  import Aggregator._

  def receive = {
    case SizeOf(url, size) =>
      log.info(s"$url has size: $size")
    case NumberOfLinks(url, links) =>
      log.info(s"$url has $links hyperlinks")
    case LinkSteps(links) =>
      links.map{ link => sender() ! Sizer.RecursiveSize(link) }
    case _ => log.info("Bad message received")
  }
}


// Sizer does the work of getting a URL and doing work with it
object Sizer {
  case class SizeOf(url: String)
  case class CountLinks(url: String)
  case class RecursiveSize(url: String)

  def props(aggregator: ActorRef): Props = Props(new Sizer(aggregator))
}

class Sizer(aggregator: ActorRef) extends Actor with ActorLogging {
  import Sizer._

  def receive = {
    case SizeOf(url) =>
      aggregator ! Aggregator.SizeOf(url, HTML.getPage(url).length)
    case CountLinks(url) =>
      aggregator ! Aggregator.NumberOfLinks(url, HTML.getNumberOfLinks(url))
    case RecursiveSize(url) => {
      aggregator ! Aggregator.SizeOf(url, HTML.getPage(url).length)
      aggregator ! Aggregator.LinkSteps(HTML.getLinks(url))
    }
    case _ => log.info("Bad message received")
  }
}


// The HTML object captures ways to work with HTML
object HTML {
  def getPage(url: String): String = {
    import java.nio.charset.CodingErrorAction

    implicit val codec = Codec("UTF-8")
    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)
    io.Source.fromURL(url).mkString
  }

  // Trying to get HTML links using the native XML tooling is evil
  // and painful. Let's use regular expressions (!!) instead.
  def getLinks(url: String): List[String] = {
    val hrefRegex = "href=\"(http://[:%a-zA-Z\\d/\\?&\\.]+)\"".r
    hrefRegex.findAllIn(getPage(url)).map{ o =>
      o match {
        case hrefRegex(link) => {
          link
        }
      }
    }
    .toList
  }

  def getNumberOfLinks(url: String): Int = getLinks(url).length
}


trait Greeting {
  lazy val greeting: String = "hello"
}
