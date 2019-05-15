package fixture

import java.time.LocalDateTime
import java.util.UUID
import model._
import anorm._
import anorm.SqlParser._
import anorm.RowParser
import java.time.Instant
import play.api.libs.json.Json

import anorm.SQL
import play.api.db.Database

object database {

  def createTree(db: Database, id: Int, treeInput: TreeInput): Unit = {
    val root: String = Json.stringify(Json.toJson(treeInput))

    val sql = SQL"""
      INSERT INTO tree(id, root) VALUES(1, $root::json);
    """
    db.withConnection(connection => sql.execute()(connection))
  }
}
