package controllers

import javax.inject.Inject
import play.api.mvc._
import play.api.mvc.{Action, Controller}
import play.api.libs.json._
import play.api.cache._
import model._
import service._

class RequestController @Inject()(
  cc: ControllerComponents,
  cache: SyncCacheApi,
  requestBuilderRepo: RequestBuilderRepo,
) extends AbstractController(cc) {

  def create = Action(parse.json) { request =>
    val tree = requestBuilderRepo.create()
    Ok(Json.toJson(tree))
  }

  def update(id: Int) = Action(parse.json) { request =>
    request.body.validate[TreeInput].map { case treeInput =>
      val newTree = requestBuilderRepo.update(id, treeInput)
      val json = Json.toJson("coucou")
      Ok(json)
    }.recoverTotal { e =>
      val error = "Detected error:" + e
      BadRequest(error)
    }
  }
}
