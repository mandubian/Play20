package play.core.server.netty

import org.jboss.netty.channel._
import org.jboss.netty.handler.codec.http._

import play.api.mvc._
import play.api.libs.iteratee._
import play.api.libs.iteratee.Input._

import scala.collection.JavaConverters._
import collection.immutable.TreeMap
import play.core.utils.CaseInsensitiveOrdered

private[netty] trait Helpers {

  def socketOut[A](ctx: ChannelHandlerContext)(frameFormatter: play.api.mvc.WebSocket.FrameFormatter[A]): Iteratee[A, Unit] = {
    val channel = ctx.getChannel()
    val nettyFrameFormatter = frameFormatter.asInstanceOf[play.core.server.websocket.FrameFormatter[A]]

    def step(future: Option[ChannelFuture])(input: Input[A]): Iteratee[A, Unit] =
      input match {
        case El(e) => Cont(step(Some(channel.write(nettyFrameFormatter.toFrame(e)))))
        case e @ EOF => future.map(_.addListener(ChannelFutureListener.CLOSE)).getOrElse(channel.close()); Done((), e)
        case Empty => Cont(step(future))
      }

    Enumeratee.breakE[A](_ => !channel.isConnected())(play.core.Execution.internalContext).transform(Cont(step(None)))
  }

  // reflection stuff done once
  import java.lang.reflect.Field
  val clazzHttpRequest = java.lang.Class.forName("org.jboss.netty.handler.codec.http.DefaultHttpMessage")
  val fieldHeaders = clazzHttpRequest.getDeclaredField("headers")
  fieldHeaders.setAccessible(true)

  val clazzHttpHeaders = java.lang.Class.forName("org.jboss.netty.handler.codec.http.HttpHeaders")
  val fieldEntries = clazzHttpHeaders.getDeclaredField("entries")
  fieldEntries.setAccessible(true)

  def getHeaders(nettyRequest: HttpRequest): Headers = {
    // reflecting to get headers entries
    val headers = fieldHeaders.get(nettyRequest)
    val entries = fieldEntries.get(headers).asInstanceOf[Array[java.util.Map.Entry[String, String]]]

    // building a vector from entries
    val builder = new scala.collection.immutable.VectorBuilder[(String, Seq[String])]()
    entries.foreach{ entry => if(entry != null) builder += (entry.getKey -> Seq(entry.getValue)) }
    val pairs = builder.result

    new Headers { val data = pairs.toSeq }
  }

  def getCookies(nettyRequest: HttpRequest): Cookies = {

    val cookies: Map[String, play.api.mvc.Cookie] = getHeaders(nettyRequest).get(play.api.http.HeaderNames.COOKIE).map { cookiesHeader =>
      new CookieDecoder().decode(cookiesHeader).asScala.map { c =>
        c.getName -> play.api.mvc.Cookie(
          c.getName, c.getValue, if (c.getMaxAge == Integer.MIN_VALUE) None else Some(c.getMaxAge),
          Option(c.getPath).getOrElse("/"), Option(c.getDomain), c.isSecure, c.isHttpOnly)
      }.toMap
    }.getOrElse(Map.empty)

    new Cookies {
      def get(name: String) = cookies.get(name)
      override def toString = cookies.toString
    }
  }
}
