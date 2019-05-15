package model

import play.api.libs.json._

case class Tree(
  id: Int,
  root: Seq[Node],
)

case class TreeInput(
  root: Seq[Node]
)

object Tree {
  implicit val format = Json.format[Tree]
}

object TreeInput {
  implicit val format = Json.format[TreeInput]
}
