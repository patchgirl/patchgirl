package model

import play.api.libs.json._

case class Tree(
  root: Seq[Node]
)

object Tree {
  implicit val format = Json.format[Tree]
}
