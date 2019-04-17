package helpers

import play.api.Configuration

case class Auth0Config(secret: String, clientId: String, callbackURL: String, domain: String, audience: String)

object Auth0Config {

  def get(configuration: Configuration): Auth0Config = {
    Auth0Config(
      configuration.getString("auth0.clientSecret").get,
      configuration.getString("auth0.clientId").get,
      configuration.getString("auth0.callbackURL").get,
      configuration.getString("auth0.domain").get,
      configuration.getString("auth0.audience").get
    )
  }
}
