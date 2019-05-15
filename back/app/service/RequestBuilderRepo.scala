package service

import play.api.db.Database
import javax.inject.Inject
import model._
import anorm._
import anorm.SqlParser.get

class RequestBuilderRepo @Inject()(database: Database) {

  def upsert(id: Int, tree: Tree): Tree = {
    database.withTransaction { implicit connection =>
      SQL("""
        UPDATE tree
        SET value = {value}
        WHERE id = {id}
        RETURNING value
      """).on(
        "id" -> id,
        "value" -> tree.toString
      ).as(RequestBuilderRepo.treeMapper.single)
    }
  }

}

object RequestBuilderRepo {

  def treeMapper: RowParser[Tree] = {
    for {
      id <- get[String]("value")
    } yield ???
  }
}
