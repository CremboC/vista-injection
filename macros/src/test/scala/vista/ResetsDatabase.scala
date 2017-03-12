package vista

import org.scalatest._

import scala.collection.mutable

/**
  * @author Paulius Imbrasas
  */
trait ResetsDatabase extends Suite with BeforeAndAfterEach {
  override def afterEach(): Unit = {
    // use reflection to avoid create a clear method
    // which shouldn't be required in normal operation

    import scala.reflect.runtime.{universe => ru}
    implicit val db = semantics.Database

    val m = ru.runtimeMirror(db.getClass.getClassLoader)
    val prop = ru.typeOf[semantics.Database.type].decl(ru.TermName("database")).asTerm

    val im = m.reflect(db)
    im.reflectField(prop).set(mutable.Set.empty)
  }
}
