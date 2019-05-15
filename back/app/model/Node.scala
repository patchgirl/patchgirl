package model

import play.api.libs.json._

sealed trait Node

case class FileNode(
  name: String,
  requestBuilder: RequestBuilder
) extends Node

case class FolderNode(
  name: String,
  open: Boolean,
  children: Seq[Node]
) extends Node

object FileNode {
  implicit val format = Json.format[FileNode]
}

object FolderNode {
  implicit val format = Json.format[FolderNode]
}

object Node {

  implicit val format: Format[Node] = new Format[Node] {
    def reads(json: JsValue): JsResult[Node] = {
      (json \ "type").validate[String] match {
        case JsSuccess("file", _) => Json.fromJson[FileNode](json)
        case JsSuccess("folder", _) => Json.fromJson[FolderNode](json)
        case _ => JsError(s"Unknown type")
      }
    }

    def writes(node: Node): JsValue = {
      val (kind: String, prod: Product, sub: JsObject) = node match {
        case b: FileNode => ("file", b, Json.toJson(b)(FileNode.format))
        case b: FolderNode => ("folder", b, Json.toJson(b)(FolderNode.format))
      }
      sub.deepMerge(JsObject(Seq("type" -> JsString(kind))))
    }
  }
}
