package controllers

import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.test._
import play.api.test.Helpers._

class RequestControllerSpec extends PlaySpec with GuiceOneAppPerTest with Injecting {

  "RequestController GET" should {

    "render the index page from the router" in {
      val request = FakeRequest(PUT, "/requests")
      val home = route(app, request).get

      status(home) mustBe OK
      contentType(home) mustBe Some("application/json")
      contentAsString(home) must include ("coucou")
    }
  }
}
