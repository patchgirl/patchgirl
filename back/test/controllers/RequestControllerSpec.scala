package controllers

import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.test._
import play.api.test.Helpers._
import play.api.mvc.Headers

class RequestControllerSpec extends PlaySpec with GuiceOneAppPerTest with Injecting {

  "RequestController GET" should {

    "render the index page from the router" in {
      val body = """
          {
            "root" : [
              {
                "type": "file",
                "name": "file2",
                "requestBuilder": {
                  "name": "myName",
                  "url": "myUrl",
                  "body": "myBody"
                }
              }
            ]
          }
        """
      val request = FakeRequest(
        method = PUT,
        uri = controllers.routes.RequestController.update().url,
        headers = FakeHeaders(Seq(("Content-type", "application/json"))),
        body = body
      )
      val response = route(app, request).get

      status(response) mustBe OK
      contentType(response) mustBe Some("application/json")
      contentAsString(response) must include ("coucou")
    }
  }
}
