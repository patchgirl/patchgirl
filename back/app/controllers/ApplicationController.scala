package controllers

import javax.inject.Inject
import play.api.cache._
import play.api.mvc._
import helpers.Auth0Config
import java.util.UUID.randomUUID
import service._
import play.api.Configuration

class ApplicationController @Inject()(
  cc: ControllerComponents,
  cache: SyncCacheApi,
  auth0Service: Auth0Service
) extends AbstractController(cc) {

  private val auth0Info = auth0Service.auth0Info

  def index = Action {
    Ok(views.html.index())
  }

  def login = Action {
    val id = randomUUID().toString
    cache.set(id + "state", auth0Info.state)
    Redirect(auth0Info.loginUrl).withSession("id" -> id)
  }

  def logout = Action {
    Redirect(auth0Info.logoutUrl).withNewSession
  }
}
