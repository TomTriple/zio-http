package example

import zio.{ZIO, ZIOAppDefault}

import zio.http._

object ClientServer extends ZIOAppDefault {

  val app = Http.collectZIO[Request] {
    case Method.GET -> !! / "hello" =>
      ZIO.succeed(Response.text("hello"))

    case Method.GET -> !! =>
      val url = "http://localhost:8080/hello"
      Client.request(url)
  }

  val run = {
    Server.serve(app.withDefaultErrorResponse).provide(Server.default, Client.default).exitCode
  }
}
