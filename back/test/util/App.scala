package util

import play.api.Application
import play.api.db.evolutions._
import play.api.db.Database
import play.api.http.Writeable
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.{ Request, Result }
import play.api.test.Helpers.route

import scala.concurrent.{ ExecutionContext, Future }

object App {

  def withApp(block: (Application, Database) => Any) = {
    val app = new GuiceApplicationBuilder().configure().build()
    val db = app.injector.instanceOf[Database]
    Evolutions.cleanupEvolutions(db)
    Evolutions.applyEvolutions(db)
    block(app, db)
    db.shutdown()
  }
}
