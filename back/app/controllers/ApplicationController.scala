package controllers

import javax.inject.Inject
import play.api.cache._
import play.api.mvc._
import helpers.Auth0Config
import java.security.SecureRandom
import java.math.BigInteger
import java.util.UUID.randomUUID

import play.api.Configuration

class ApplicationController @Inject() (cc: ControllerComponents, cache: SyncCacheApi, configuration: Configuration) extends AbstractController(cc) {

  private val config = Auth0Config.get(configuration)

  def index = Action {
    Ok(views.html.index())
  }

  def login = Action {
    // Generate random state parameter
    object RandomUtil {
      private val random = new SecureRandom()

      def alphanumeric(nrChars: Int = 24): String = {
        new BigInteger(nrChars * 5, random).toString(32)
      }
    }
    val state = RandomUtil.alphanumeric()

    var audience = config.audience
    if (config.audience == ""){
      audience = String.format("https://%s/userinfo", config.domain)
    }

    val id = randomUUID().toString
    cache.set(id + "state", state)
    val query = String.format(
      "authorize?client_id=%s&redirect_uri=%s&response_type=code&scope=openid profile&audience=%s&state=%s",
      config.clientId,
      config.callbackURL,
      audience,
      state
    )
    Redirect(String.format("https://%s/%s", config.domain, query)).withSession("id" -> id)
  }

  def logout = Action {
    Redirect(String.format(
      "https://%s/v2/logout?client_id=%s&returnTo=http://localhost:3000",
      config.domain,
      config.clientId)
    ).withNewSession
  }
}
