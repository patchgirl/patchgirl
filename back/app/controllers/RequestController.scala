package controllers

import javax.inject.Inject
import play.api.mvc._
import play.api.mvc.{Action, Controller}
import play.api.libs.json._
import play.api.cache._

class RequestController @Inject()(cc: ControllerComponents, cache: SyncCacheApi) extends AbstractController(cc) {

  def update = Action { implicit request =>
    val json = Json.toJson("coucou")
    Ok(json)
 }
}
