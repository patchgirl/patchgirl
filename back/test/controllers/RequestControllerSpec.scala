package controllers

import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.test._
import play.api.test.Helpers._
import play.api.mvc.Headers
import util.App._
import fixture._
import model._

class RequestControllerSpec extends PlaySpec with GuiceOneAppPerTest with Injecting {

  "PUT /requests/:id" should {

    "update a tree" in withApp { case (_, db) =>
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
      database.createTree(db, 1, TreeInput(Seq()))

      val request = FakeRequest(
        method = PUT,
        uri = controllers.routes.RequestController.update(1).url,
        headers = FakeHeaders(Seq(("Content-type", "application/json"))),
        body = body
      )
      val response = route(app, request).get

      status(response) mustBe OK
      contentType(response) mustBe Some("application/json")
      contentAsString(response) must include ("coucou")
    }
  }

  "POST /requests" should {

    "create a tree" in withApp { case (_, _) =>
      val request = FakeRequest(
        method = POST,
        uri = controllers.routes.RequestController.create().url,
        headers = FakeHeaders(Seq(("Content-type", "application/json"))),
        body = "{}",
      )
      val response = route(app, request).get

      status(response) mustBe OK
      contentType(response) mustBe Some("application/json")
      contentAsString(response) mustBe """{"id":1,"root":[]}"""
    }
  }
}
