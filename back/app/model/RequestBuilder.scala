package model

import play.api.libs.json._

/*sealed trait Scheme
case object HttpScheme extend Scheme
case object HttpsScheme extend Scheme
*/

case class RequestBuilder(
  name: String,
  url: String,
  body: String
)

object RequestBuilder {
  implicit val format = Json.format[RequestBuilder]
}
