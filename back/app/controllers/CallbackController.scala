package controllers

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import javax.inject.Inject
import play.api.cache._
import play.api.http.HeaderNames
import play.api.http.MimeTypes
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.ws._
import play.api.Configuration

import service._
import play.api.mvc._
import helpers.Auth0Config

class Callback @Inject()(
  cc: ControllerComponents,
  cache: SyncCacheApi,
  auth0Service: Auth0Service,
  configuration: Configuration,
  ws: WSClient
) extends AbstractController(cc) {

  def callback(codeOpt: Option[String], stateOpt: Option[String]) = Action.async { request =>
    val sessionId = request.session.get("id").get
    if (stateOpt == cache.get(sessionId + "state")) {
      (for {
        code <- codeOpt
      } yield {
        auth0Service.getToken(code, sessionId).flatMap { case (idToken, accessToken) =>
          auth0Service.getUserInfo(accessToken).map { user =>
            cache.set(request.session.get("id").get + "profile", user)
            Redirect(routes.UserController.index())
          }
        }.recover {
          case ex: IllegalStateException => Unauthorized(ex.getMessage)
        }
      }).getOrElse(Future.successful(BadRequest("No parameters supplied")))
    } else {
      Future.successful(BadRequest("Invalid state parameter"))
    }
  }
}
