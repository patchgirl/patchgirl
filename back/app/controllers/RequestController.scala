package controllers

import javax.inject.Inject
import play.api.mvc._
import play.api.mvc.{Action, Controller}
import play.api.libs.json._
import play.api.cache._
import model._

class RequestController @Inject()(cc: ControllerComponents, cache: SyncCacheApi) extends AbstractController(cc) {

  def update = Action(parse.json) { request =>
    request.body.validate[Tree].map { case node =>
      println(node)
      val json = Json.toJson("coucou")
      Ok(json)
    }.recoverTotal { e =>
      val error = "Detected error:" + e
      BadRequest(error)
    }
  }
}
