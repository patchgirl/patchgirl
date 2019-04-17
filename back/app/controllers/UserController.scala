package controllers

import javax.inject.Inject
import play.api.mvc._
import play.api.mvc.{Action, Controller}
import play.api.libs.json._
import play.api.cache._

class UserController @Inject()(cc: ControllerComponents, cache: SyncCacheApi) extends AbstractController(cc) {

  def AuthenticatedAction(f: (Request[AnyContent], JsValue) => Result): Action[AnyContent] = Action { request =>
    (for {
      id <- request.session.get("id")
      profile <- cache.get[JsValue](id + "profile")
    } yield f(request, profile)).getOrElse {
      Redirect(routes.ApplicationController.index())
    }
  }

  def index = AuthenticatedAction { case (request, profile) =>
    val id = request.session.get("id").get
    Ok(views.html.user(profile))
  }
}
