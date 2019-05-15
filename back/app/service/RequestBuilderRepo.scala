package service

import play.api.db.Database
import javax.inject.Inject
import model._
import anorm._
import anorm.SqlParser.get
import play.api.libs.json.JsValue
import play.api.libs.json.JsValue._
import play.api.libs.json.Json

class RequestBuilderRepo @Inject()(database: Database) {

  def create(): Tree = {
    database.withTransaction { implicit connection =>
      SQL("""
        INSERT INTO tree (root)
        VALUES ({root}::json)
        RETURNING *
      """).on(
        "root" -> """{ "root": [] }"""
      ).as(RequestBuilderRepo.treeMapper.single)
    }
  }

  def update(id: Int, treeInput: TreeInput): Tree = {
    database.withTransaction { implicit connection =>
      SQL("""
        UPDATE tree
        SET root = {root}::json
        WHERE id = {id}
        RETURNING *
      """).on(
        "id" -> id,
        "root" -> Json.stringify(Json.toJson(treeInput))
      ).as(RequestBuilderRepo.treeMapper.single)
    }
  }
}

object RequestBuilderRepo {

  import anorm._
  import anorm.SqlParser._
  import play.api.libs.json._
  import org.postgresql.util.PGobject

  implicit def rowToJsValue: Column[JsValue] = Column.nonNull { (value, meta) =>
    val MetaDataItem(qualified, nullable, clazz) = meta
    value match {
      case pgo: PGobject => Right({
        pgo.getType match {
          case "json" => Json.parse(pgo.getValue)
          case _ => JsNull
        }
      })
      case _ => Left(TypeDoesNotMatch("Cannot convert " + value + ":" +
          value.asInstanceOf[AnyRef].getClass + " to JsValue for column " + qualified))
    }
  }

  def treeMapper: RowParser[Tree] = {
    for {
      id <- get[Int]("id")
      root <- get[JsValue]("root").map(json => (json \ "root").as[Seq[Node]])
    } yield Tree(id, root)
  }
}
