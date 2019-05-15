package service

import javax.inject.Inject
import java.security.SecureRandom
import java.math.BigInteger
import play.api.Configuration
import helpers._
import scala.concurrent.Future
import play.api.libs.ws._
import play.api.http.HeaderNames
import play.api.http.MimeTypes
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.cache._
import scala.concurrent.ExecutionContext.Implicits.global

class Auth0Service @Inject()(
  configuration2: Configuration,
  cache: SyncCacheApi,
  ws: WSClient,
) {

  private val auth0Config = Auth0Config.get(configuration2)

  case class Auth0Info(state: String, audience: String, loginUrl: String, logoutUrl: String)

  def auth0Info(): Auth0Info = {
    object RandomUtil {
      private val random = new SecureRandom()

      def alphanumeric(nrChars: Int = 24): String = {
        new BigInteger(nrChars * 5, random).toString(32)
      }
    }
    val state = RandomUtil.alphanumeric()

    val audience = auth0Config.audience match {
      case "" => String.format("https://%s/userinfo", auth0Config.domain)
      case nonEmptyString => nonEmptyString
    }

    val query = String.format(
      "authorize?client_id=%s&redirect_uri=%s&response_type=code&scope=openid profile&audience=%s&state=%s",
      auth0Config.clientId,
      auth0Config.callbackURL,
      audience,
      state
    )

    val loginUrl = String.format("https://%s/%s", auth0Config.domain, query)
    val logoutUrl = String.format(
      "https://%s/v2/logout?client_id=%s&returnTo=http://localhost:9000",
      auth0Config.domain,
      auth0Config.clientId
    )

    Auth0Info(state, audience, loginUrl, logoutUrl)
  }

  def getToken(code: String, sessionId: String): Future[(String, String)] = {
    val audience = auth0Config.audience match {
      case "" => String.format("https://%s/userinfo", auth0Config.domain)
      case nonEmptyString => nonEmptyString
    }

    val tokenResponse = ws.url(String.format("https://%s/oauth/token", auth0Config.domain)).
      withHeaders(HeaderNames.ACCEPT -> MimeTypes.JSON).
      post(
        Json.obj(
          "client_id" -> auth0Config.clientId,
          "client_secret" -> auth0Config.secret,
          "redirect_uri" -> auth0Config.callbackURL,
          "code" -> code,
          "grant_type"-> "authorization_code",
          "audience" -> audience
        )
      )

    tokenResponse.flatMap { response =>
      (for {
        idToken <- (response.json \ "id_token").asOpt[String]
        accessToken <- (response.json \ "access_token").asOpt[String]
      } yield {
        cache.set(sessionId + "id_token", idToken)
        cache.set(sessionId + "access_token", accessToken)
        Future.successful((idToken, accessToken))
      }).getOrElse(Future.failed[(String, String)](new IllegalStateException("Tokens not sent")))
    }
  }

  def getUserInfo(accessToken: String): Future[JsValue] = {
    ws.url(String.format("https://%s/userinfo", auth0Config.domain))
      .withQueryString("access_token" -> accessToken)
      .get()
      .map(_.json)
  }
}
